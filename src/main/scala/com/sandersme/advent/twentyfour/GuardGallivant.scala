package com.sandersme.advent.twentyfour

import java.security.Guard
import com.sandersme.advent.Input
import com.sandersme.advent.twentytwo.model.CommDecoder.loopIncrementCharacters
import com.sandersme.advent.twentytwo.model.Direction


enum Direction {
  case Left, Up, Right, Down
}

enum PosType {
  case Obstacle, Guard, Empty
}

case class Line(start: Point, end: Point, direction: Direction) 

case class GuardGallivant(rows: Map[Int, List[Int]], cols: Map[Int, List[Int]], height: Int,
  width: Int, guard: Point, direction: Direction, visitedPoints: Set[Point], lines: Set[Line], start: Point) {
    val OFFBOARD_POINT = Point(-1, -1)
    val CONTAINS_LOOP_POINT = Point(-2, -2)

    def moveGuard: GuardGallivant = {
      val (pointsTraveled: Seq[Point], endPoint: Option[Point]) = direction match {
        case Direction.Up => {
          val startY = cols.getOrElse(guard.x, List.empty).findLast(_ < guard.y).getOrElse(-1)

          val points = GuardGallivant.generatePoints(guard.x, guard.x + 1, startY + 1, guard.y)
          (points, points.headOption)
        }
        case Direction.Right => {
          val endX = rows.getOrElse(guard.y, List.empty).find(_ > guard.x).getOrElse(width)

          val points =GuardGallivant.generatePoints(guard.x, endX, guard.y, guard.y + 1)
          (points, points.lastOption)
        }
        case Direction.Down => {
          val endY = cols.getOrElse(guard.x, List.empty).find(_ > guard.y).getOrElse(height)

          val points = GuardGallivant.generatePoints(guard.x, guard.x + 1, guard.y, endY)
          (points, points.lastOption)
        }
        case Direction.Left => {
          val startX = rows.getOrElse(guard.y, List.empty).findLast(_ < guard.x).getOrElse(-1)
         
          val points = GuardGallivant.generatePoints(startX + 1, guard.x, guard.y, guard.y + 1)
          (points, points.headOption)
        }
      }

      val line = Line(guard, endPoint.getOrElse(guard), direction)

      val guardEnd = endPoint
        .map{ case endPosition => 
          if (endPosition.x == 0 || endPosition.y == 0 || endPosition.x == width - 1 || endPosition.y == height - 1) {
            OFFBOARD_POINT 
          } else if (lines.contains(line)) {
            println(line)
            CONTAINS_LOOP_POINT
          }else { 
            endPosition 
          } 
        }.getOrElse(OFFBOARD_POINT)

      val nextDirection = direction match {
        case Direction.Up => Direction.Right
        case Direction.Right => Direction.Down
        case Direction.Down => Direction.Left
        case Direction.Left => Direction.Up
      }

      val updatedVisited = visitedPoints ++ pointsTraveled
    
      this.copy(visitedPoints = updatedVisited, direction = nextDirection, guard = guardEnd, lines = lines + line)
    }

    /* 
     * What I can do is once I have all the visited locations. I go through them each one
     * at a time. Then place the guard back at the beginning and see if it creates a loop)
     * if it does create a loop then I can sum by truthy
     * */
    def countNumLoops: Int = { 
      visitedPoints
        .filter(_ != start)
        .count { point => 
          val updatedRows = rows + ((point.y, (rows.getOrElse(point.y, List.empty) :+ point.x).sorted))
          val updatedCols = cols + ((point.x, (cols.getOrElse(point.x, List.empty) :+ point.y).sorted))
          val guardGallivant = this.copy(rows = updatedRows, cols = updatedCols,
            guard = start, visitedPoints = Set(start), direction = Direction.Up, lines = Set.empty)

          val updatedGuard = GuardGallivant.moveGuardTillOffboard(guardGallivant)
          updatedGuard.hasLoop
        }
    }

    def isFinished: Boolean = offBoard || hasLoop

    def offBoard: Boolean = guard == OFFBOARD_POINT
    def hasLoop: Boolean = guard == CONTAINS_LOOP_POINT

  }


object GuardGallivant {
  
  def moveGuardTillOffboard(guard: GuardGallivant): GuardGallivant = {
    // THE FOLLOWING PRINT STATEMENTS IS TO VISUALIZE THE MOVE THROUGH THE MAZE. 
    // print("\u001b[2J")
    // printVisited(guard)
    // println(guard.guard.toString() + "   -- DIR: " + guard.direction )
    // Thread.sleep(200)
    if (guard.isFinished) {
      guard
    } else {
      moveGuardTillOffboard(guard.moveGuard)
    }
  }

  def generatePoints(startX: Int, endX: Int, startY: Int, endY: Int): Seq[Point] = {
    for {
      x <- startX until endX
      y <- startY until endY
    } yield Point(x, y)
  }

  def main(args: Array[String]): Unit = {
    val timeStart = System.currentTimeMillis()
    val input = Input.readTwentyFourFromResource("day6_input")
    val guardGallivant = GuardGallivant.parseInput(input) 

    val moved = GuardGallivant.moveGuardTillOffboard(guardGallivant)
    val timeStop = System.currentTimeMillis()

    println("Time to process grid: " + (timeStop - timeStart))
    println("Final size of the visited Spaces Equals: " + moved.visitedPoints.size)
    println("Size of grid: " + (guardGallivant.height * guardGallivant.width)) 

    val timeStartObstacles = System.currentTimeMillis
    val numObstaclesLoops = moved.countNumLoops
    val totalTimeObstacles = System.currentTimeMillis() - timeStartObstacles

    println(f"Total time to process grid part 2: $totalTimeObstacles")
    println(f"Total Num Obstacles that create Loops: $numObstaclesLoops")
  }


  def printVisited(guard: GuardGallivant): Unit = {
    val rowPoints = guard.rows.flatMap { case (y, values) => values.map(x => Point(x, y)) }
    val colPoints = guard.cols.flatMap { case (x, values) => values.map(y => Point(x, y)) } 
    val obstacles = (rowPoints ++ colPoints).toSet

     (0 until guard.width).foreach { y => 
      (0 until guard.height).foreach{ x =>
        if (guard.visitedPoints.contains(Point(x, y))) {
          // print("^");
          print(".")
         } else if (obstacles.contains(Point(x, y))) {
          print("#")
         } else
          print(".");
        }
        println("")
      }
  }

  def parseInput(in: List[String]): GuardGallivant = {
    val height = in.length
    val width = in.head.length

    val grid = for {
      x <- 0 until width
      y <- 0 until height
      
      value = in(y).charAt(x)
      obstacle = value match {
        case '#' => PosType.Obstacle
        case '^' => PosType.Guard
        case _ => PosType.Empty
      }

      if (obstacle != PosType.Empty)
    } yield (x, y, obstacle)

    type ResType = (Map[Int, List[Int]], Map[Int, List[Int]], (Int, Int))

    val (cols, rows, guard) = grid.foldLeft[ResType](Map.empty, Map.empty, (0, 0)) { case ((xaxis, yaxis, pos), (x, y, t)) =>
      t match {
        case PosType.Guard => (xaxis, yaxis, (x, y))
        case PosType.Obstacle => {
          val updatedX = xaxis.getOrElse(x, List.empty) :+ y
          val updatedY = yaxis.getOrElse(y, List.empty) :+ x

          (xaxis + ((x, updatedX)), yaxis + ((y, updatedY)), pos)
        }
        case _ => throw new Exception("ERROR Should not get to this state")
      } 
    }

    val guardPos = Point(guard._1, guard._2)
    GuardGallivant(rows, cols, height, width, guardPos, Direction.Up, Set(guardPos), Set.empty, guardPos)
  }
  
}
