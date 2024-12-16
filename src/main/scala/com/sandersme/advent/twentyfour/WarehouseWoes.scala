package com.sandersme.advent.twentyfour

import com.sandersme.advent.twentyfour.Point
import com.sandersme.advent.twentyfour.Direction.{Left, Right, Up, Down }
import com.sandersme.advent.twentyone.model.DiceBoardGame.WINNING_SCORE
import scala.annotation.tailrec
import com.sandersme.advent.Input


enum TObject {
  case LeftBox, RightBox, TOpen, TWall
}

case class WarehouseWoesTwo(objects: Vector[Vector[TObject]], robot: Point, directions: List[List[Direction]]) {
  import TObject.*

  def moveRobotPartTwo: WarehouseWoesTwo = {
    val (finalObjects, finalRobot) = directions.flatten
      .foldLeft((objects, robot)){ case ((currObjects, currRobot), nextDir) =>

      // WarehouseWoes.printPartTwo(currObjects, currRobot)
     
      
      // println(f"$currRobot --- $nextDir")
      val nextPoint = WarehouseWoes.movePoint(currRobot, nextDir)
      val nextObj = WarehouseWoes.objectAtTwo(currObjects, nextPoint.x, nextPoint.y)
      
      // println("moving robot at: " + currRobot + " in " + nextDir + " to a " + nextObj + " at " + nextPoint)
      nextObj match {
        case TWall => (currObjects, currRobot)
        case TOpen => (currObjects, nextPoint)
        case  _ => WarehouseWoes.moveAllBoxesTwo(currObjects, currRobot, nextDir)
      }
    }

    this.copy(finalObjects, finalRobot)
  }

  def calculateFinalScorePartTwo: Long = {

    objects.zipWithIndex .flatMap{ (line, y) =>
      line.zipWithIndex.map { (obj, x) => 
        obj match {
          case LeftBox => (100 * y) + x 
          case _ => 0
        }
      }
    }.sum
  }
}

case class WarehouseWoes(objects: Vector[Vector[WObject]], robot: Point, directions: List[List[Direction]]) {
  def objectAt(x: Int, y: Int): WObject =  {
    WarehouseWoes.objectAt(objects, x, y)
  }

  def calculateFinalScore: Long = {
    WarehouseWoes.calculateFinalScore(objects)
  }
  /**
      * If the tile is #, the new map contains ## instead.
        If the tile is O, the new map contains [] instead.
        If the tile is ., the new map contains .. instead.
        If the tile is @, the new map contains @. instead.
      */
  def expandGridPartTwo: WarehouseWoesTwo = {
    val partTwoObjects =    objects.map(_.flatMap{ _ match {
      case Wall => List(TObject.TWall, TObject.TWall)
      case Open => List(TObject.TOpen, TObject.TOpen)
      case Box => List(TObject.LeftBox, TObject.RightBox)
      }
    })
    
    val updatedRobotPart2 = robot.copy(x = robot.x * 2)
    WarehouseWoesTwo(partTwoObjects, updatedRobotPart2, directions)
  }
  
  def printGrid: Unit = {
    WarehouseWoes.printGrid(objects, robot)
  }


  def moveRobotPartOne: WarehouseWoes = {
    val (finalGrid, finalRobot) = directions.flatten
      .foldLeft((objects, robot)) { case ((updatedObjects, rPoint), nextDir) =>

      val nextPoint = WarehouseWoes.movePoint(rPoint, nextDir)
      val nextObject = WarehouseWoes.objectAt(updatedObjects, nextPoint.x, nextPoint.y)
      
      val (robotEndPoint, boxesToMove) = nextObject match {
        case Wall => (rPoint, List.empty) // Straight forward a robot can't move 
        case Open => (nextPoint, List.empty) // Robot can move to that point.
        case Box => { 
          val movableBoxes = WarehouseWoes.findAllMovableBoxes(updatedObjects, nextPoint, nextDir, List.empty)
          if  (movableBoxes.isEmpty) {
            (rPoint, movableBoxes)
          } else {
            (nextPoint, movableBoxes)
          }
        }// This one is complex because we'll also have to move boxes
      }
      // println(f"Robot Start: $rPoint Dir: $nextDir ENdPoint: $robotEndPoint Objet: $nextObject boxesToMove $boxesToMove")

      val finalGrid = WarehouseWoes.moveAllBoxes(updatedObjects, boxesToMove, nextDir)
      // WarehouseWoes.printGrid(finalGrid, robotEndPoint)

      (finalGrid, robotEndPoint)
    }

    this.copy(objects = finalGrid, robot = finalRobot)
  }

}



sealed trait WObject
case class WRobot(point: Point)
case object Wall extends  WObject
case object Box extends WObject
case object Open extends WObject

object WarehouseWoes {


  def calculateFinalScore(objects: Vector[Vector[WObject]]): Long = {
    val allObjectScores = for {
      y <- 0 until objects.length
      x <- 0 until objects(0).length

      if (objectAt(objects, x, y) == Box)
    } yield (100L * y) + x
    
    allObjectScores.sum
  }

  def printPartTwo(objects: Vector[Vector[TObject]], robot: Point): Unit = {
    objects.zipWithIndex.foreach{ (line, y) => 
      line.zipWithIndex.foreach{ (charac, x) => charac match 
        case _ if robot.x == x && robot.y == y => print('@') 
        case TObject.LeftBox => print('[')
        case TObject.RightBox => print(']')
        case TObject.TWall => print('#')
        case TObject.TOpen => print('.')
      }
      println("")
    }
  }

  def main(args: Array[String]): Unit = {
    val input = Input.readTwentyFourFromResource("day15_input")
    val warehouse = WarehouseWoes.parseInput(input)
    val moveRobot = warehouse.moveRobotPartOne
    val finalScore = moveRobot.calculateFinalScore
    val expandedWarehouse = warehouse.expandGridPartTwo
    val expandedFinalScore = expandedWarehouse.moveRobotPartTwo.calculateFinalScorePartTwo
    println("The final score for part 1 is: " + finalScore)
    println("The final score for part 2 is: " + expandedFinalScore)
  }

  def printGrid(grid: Vector[Vector[WObject]], robot: Point): Unit = {
    grid.zipWithIndex.foreach{ (line, y) =>
        line.zipWithIndex.foreach { (obj, x) =>
         obj match {
            case _ if (robot.x == x && robot.y == y) => print("@")
            case Open => print('.')
            case Box => print('O')
            case Wall => print('#')
           }
        }
        println("")
    }
  }
  def objectAt(objects: Vector[Vector[WObject]], x: Int, y: Int): WObject = {
    objects(y)(x)
  }

  def objectAtTwo(objects: Vector[Vector[TObject]], x: Int, y: Int): TObject = {
    objects(y)(x)
  }

  /// REALLY what I need to do is if there is a line of boxes to move. I just need to 
  // convert the head box to an open space. Then take the last box and update that space
  def moveAllBoxes(grid: Vector[Vector[WObject]], boxes: List[Point], dir: Direction): Vector[Vector[WObject]] = {
    if (boxes.isEmpty) {
      grid
    } else {
      val headBox = boxes.head 
      val nextBox = movePoint(boxes.last, dir)
      
      val openedHeadGrid = grid.updated(headBox.y, grid(headBox.y).updated(headBox.x, Open))
      val finalGrid = openedHeadGrid.updated(nextBox.y, openedHeadGrid(nextBox.y).updated(nextBox.x, Box))
      // println(f"moving $headBox -${objectAt(grid, headBox.x, headBox.y)} and $nextBox -${objectAt(grid, nextBox.x, nextBox.y)}to " +
      //   f"$headBox -${objectAt(finalGrid, headBox.x, headBox.y)} and $nextBox -${objectAt(finalGrid, nextBox.x, nextBox.y)}")

      finalGrid
    }
  }

  import TObject.*

  // We know before we get here that the nextpoint is a box, what we don't know is if it
  // can move or if it can move
  def moveAllBoxesTwo(objects: Vector[Vector[TObject]], robot: Point, dir: Direction): (Vector[Vector[TObject]], Point) = {
    def findHorizontalEndSace(currPoint: Point, results: List[Point] = List.empty): List[Point] = {
      objectAtTwo(objects, currPoint.x, currPoint.y) match {
        case TWall => List.empty
        case TOpen => results
        case _ => findHorizontalEndSace(movePoint(currPoint, dir), results :+ currPoint)
      }
    }

    def findVertical(currPoint: Point, dir: Direction, results: Set[Point], toProcess: List[Point]): Set[Point] = {
      val containsCurrentPoint = results.contains(currPoint)

      objectAtTwo(objects, currPoint.x, currPoint.y) match {
        case _ if containsCurrentPoint && toProcess.nonEmpty => 
          findVertical(toProcess.head, dir, results, toProcess.tail) 
        case _ if containsCurrentPoint => results 
        case TWall => Set.empty
        case TOpen if toProcess.isEmpty => results
        case TOpen => findVertical(toProcess.head, dir, results, toProcess.tail)
        case LeftBox => {
          val updatedToProcess = toProcess ++ List(movePoint(currPoint, Right), movePoint(currPoint, dir))
          findVertical(updatedToProcess.head, dir, results + currPoint, updatedToProcess.tail)
        }
        case RightBox => {
          val updatedToProceses = toProcess ++ List(movePoint(currPoint, Left), movePoint(currPoint, dir))
          findVertical(updatedToProceses.head, dir, results + currPoint, updatedToProceses.tail)
        }
      }
    }

    val nextRobot = movePoint(robot, dir)
    dir match {
      case Left | Right => {
        val objectsToMove = findHorizontalEndSace(nextRobot)
        if (objectsToMove.isEmpty) {
          (objects, robot)
        } else {
          (moveObjectsHorizontall(objects, objectsToMove, dir), nextRobot)
        }
      }
      case _ => {
        val objectsToMove = findVertical(nextRobot, dir, Set.empty, List.empty)
        if (objectsToMove.isEmpty) {
          (objects, robot)
        } else {
          ( moveObjectsVertically(objects, objectsToMove.toList, dir), nextRobot)
        }
      }
    }
  }

  def moveObjectsVertically(objects: Vector[Vector[TObject]], points: List[Point], dir: Direction): Vector[Vector[TObject]] = {
    // println(f"points to move vertically: $points in dir: $dir ${objectAtTwo(objects, points.head.x, points.head.y)}")
    val secondPointSpace: Point = objectAtTwo(objects, points.head.x, points.head.y) match {
      case LeftBox => movePoint(points.head, Right)
      case RightBox => movePoint(points.head, Left)
      case err: _ => throw new Exception("Error should never make it to this state " + err + " found")
    }

    val newPointObjects: List[(Point, TObject)] = points.map { point =>
      (movePoint(point, dir), objectAtTwo(objects, point.x, point.y))
    }

    val clearedOriginalPoints = points.foldLeft(objects) { case (agg, nextPoint) =>
      agg.updated(nextPoint.y, agg(nextPoint.y).updated(nextPoint.x, TOpen))
    }

    
    newPointObjects.foldLeft(clearedOriginalPoints) { case (agg, (nextPoint, nextObj)) =>
      agg.updated(nextPoint.y, agg(nextPoint.y).updated(nextPoint.x, nextObj))
    }
  }

  def moveObjectsHorizontall(objects: Vector[Vector[TObject]], points: List[Point], dir: Direction): Vector[Vector[TObject]] = {
    val newPointObjects = points.map{ point => 
      (movePoint(point, dir), objectAtTwo(objects, point.x, point.y))
    } :+ (points.head, TOpen)


    newPointObjects.foldLeft(objects) { case (agg, (nextPoint, nextObj)) => 
      agg.updated(nextPoint.y, agg(nextPoint.y).updated(nextPoint.x, nextObj))
    }
  }

  def pointBefore(point: Point, dir: Direction): Point = {
    dir match {
      case Left => Point(point.x + 1, point.y)
      case Right => Point(point.x - 1, point.y)
      case Up => Point(point.x, point.y + 1)
      case Down => Point(point.x, point.y - 1)
    }
  }


  def movePoint(point: Point, dir: Direction): Point = {
      dir match {
        case Left => Point(point.x - 1, point.y)
        case Right => Point(point.x + 1, point.y)
        case Up => Point(point.x, point.y - 1)
        case Down => Point(point.x, point.y + 1)
      }
  }

  @tailrec
  def findAllMovableBoxes(grid: Vector[Vector[WObject]], p: Point, 
    dir: Direction, res: List[Point]): List[Point] = {

    objectAt(grid, p.x, p.y) match {
      case Open => res
      case Wall => List.empty
      case Box => findAllMovableBoxes(grid, movePoint(p, dir), dir, res :+ p)
    }
  }

  def parseInput(in: List[String]): WarehouseWoes = {
    val mapString = in.takeWhile(_.trim.nonEmpty)
    val instructionsString = in.drop(mapString.size + 1)

    val parsedInstructions = parseInstructions(instructionsString) 
    val parsedWalls = parseMap(mapString)

    parsedWalls.copy(directions = parsedInstructions)
  }

  def parseMap(in: List[String]): WarehouseWoes = {
    val parsedWalls = in.map(_.map { _ match 
      case '#' => Wall
      case 'O' => Box
      case _ => Open 
    }.toVector).toVector
    
    val robot = findRobotLoc(in, 0, 0)
    // println("Parsed WALLS: ")
    // printGrid(parsedWalls, Point(-1, -1))
    WarehouseWoes(parsedWalls, robot, List.empty)
  }

  def findRobotLoc(in: List[String], x: Int, y: Int): Point = {
    if (in(y)(x) == '@')
      Point(x, y)
    else if (x + 1 == in(0).length) {
      findRobotLoc(in, 0, y + 1)
    } else {
      findRobotLoc(in, x + 1, y)
    }
  }

  def parseInstructions(in: List[String]): List[List[Direction]] = {
    val parsed = in.map(line => line.map { inst =>
      inst match {
        case '^' => Direction.Up
        case '>' => Direction.Right
        case 'v' => Direction.Down
        case '<' => Direction.Left
        case _ => throw new Exception("Error should not reach this destination")
      }
    }.toList)

    parsed
  }
}

