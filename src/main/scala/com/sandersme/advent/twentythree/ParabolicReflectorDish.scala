package com.sandersme.advent.twentythree

import com.sandersme.advent.twentyfour.Point
import com.sandersme.advent.twentythree.ParabolicReflectorDish.findNorthernMostPoint
import com.sandersme.advent.twentythree.ParabolicReflectorDish.moveRock
import com.sandersme.advent.Input
import com.sandersme.advent.twentythree.ParabolicReflectorDish.findEasternMostPoint
import com.sandersme.advent.twentythree.ParabolicReflectorDish.findWesternMostPoint
import com.sandersme.advent.twentythree.ParabolicReflectorDish.findSouthernMostPoint

import scala.collection.mutable.{Set => MSet, Map => MMap}


enum GObj { 
  case Cube, Round, Open
}
case class ParabolicReflectorDish(grid: Vector[Vector[GObj]]) {
  val height = grid.length
  val width = grid(0).length

  def objAt(point: Point): GObj = {
    grid(point.y)(point.x)
  }

  def spinNCyclesAndPrint(n: Int): Unit = {
    (0 until n).foldLeft(this) { case (current, _) => 
      val spun = current.spinOneCycle
      spun.printGrid
      println("\n\n")
      spun
    }

    println("DONESPINNING")
  }

  /// Tilt 1000 times. 
  // THEN Create a c
  def tiltAndRotate: Int = {
    val cache = MMap.empty[ParabolicReflectorDish, Int]
    val mapCache = MMap.empty[Int, ParabolicReflectorDish]
    // THe cache is going to be based on the number that we start seeing  when something
    // exists in the cache. 
    def loopTillFindRepeat(step: Int, currentDish: ParabolicReflectorDish): (ParabolicReflectorDish, Int) = {
      if (cache.contains(currentDish) ) {
        println("Cache foudn or steps hit 10001 at step " + step )
        println("Cache Size: " + cache.size + " Cache hits to step: " + cache(currentDish))
        (currentDish, step)
      } else {
        cache += (currentDish -> step)
        val next = currentDish.spinOneCycle
        mapCache += (step -> next)
        loopTillFindRepeat(step + 1, next) 
      }
    }


    val (dish, steps) = loopTillFindRepeat(1, this)

    val cycleStart = cache(dish)
    val difference = steps - cycleStart
    val d = (1000000000 - steps) % difference
    // 
    val finalDish = (0 until d).foldLeft(dish){ (dishAgg, nextStep) =>
      dishAgg.spinOneCycle
    }
    mapCache((d + cycleStart)).calculateLoad
  }

  // Right now it's just north but I imagine we'll tilt several times
  def tiltRocksNorth: ParabolicReflectorDish = {
    tiltRocksDirection(0)
  }

  def spinOneCycle: ParabolicReflectorDish = {
    tiltRocksDirection(0)
      .tiltRocksDirection(1)
      .tiltRocksDirection(2)
      .tiltRocksDirection(3)
  }

  def tiltRocksDirection(dir: Int): ParabolicReflectorDish = {
   val tiltFn = dir match {
      case 0 => findNorthernMostPoint 
      case 1 => findWesternMostPoint 
      case 2 => findSouthernMostPoint 
      case 3 => findEasternMostPoint 
   }

   val yRange = if (dir == 2 || dir == 3) (height - 1 to 0 by -1) else (0 until height)
   val xRange = if (dir == 2 || dir == 3) (width - 1 to 0 by -1) else (0 until width)

    val updatedGrid = yRange.foldLeft(grid){ case (yGrid, y) =>
      xRange.foldLeft(yGrid) { case (xGrid, x) =>
        if (xGrid(y)(x) == GObj.Round) {
          val northerOpenPoint = tiltFn(Point(x, y), xGrid) 
          moveRock(Point(x, y), northerOpenPoint, xGrid)
        } else {
          xGrid
        }
    }
  }

    ParabolicReflectorDish(updatedGrid)

 }

  def calculateLoad: Int = {
    val h  = height
    val loads = for { 
      y <- 0 until height
      x <- 0 until width

      if (grid(y)(x) == GObj.Round)  
    } yield h - y

    loads.sum
  }

  def printGrid: Unit = {
    println("GRID SIZE: " + grid.length + " " + grid.headOption.map(_.length))
    grid.foreach { line =>
      line.foreach { gObj =>
        gObj match
          case GObj.Open => print('.')
          case GObj.Cube => print('#')
          case GObj.Round => print('O')
        
      }
      println("")
    }
  }
}

object ParabolicReflectorDish {
  def main(args: Array[String]): Unit = {
    val input = Input.readTwentyThreeFromResource("day14_input")

    val parabolicDish = ParabolicReflectorDish.parseInput(input)

    val loadFromTilt = parabolicDish.tiltRocksNorth.calculateLoad

    println("The total load is: " + loadFromTilt)

    println("GOING TO Tilt N Number of times (200 to start):") 
    val startTime = System.currentTimeMillis 

    val finalLoad = parabolicDish.tiltAndRotate
    val endTime = System.currentTimeMillis - startTime
    println(f"It took ${endTime}ms to tilt numerous times. with a load: ${finalLoad}")
  }

  def moveRock(current: Point, newPoint: Point, grid: Vector[Vector[GObj]]): Vector[Vector[GObj]] = {
    val objectAtCurrent = grid(current.y)(current.x)
    val emptyCurrent = grid.updated(current.y, grid(current.y).updated(current.x, GObj.Open))
    
    emptyCurrent.updated(newPoint.y, emptyCurrent(newPoint.y).updated(newPoint.x, objectAtCurrent))
  }

  def findNorthernMostPoint(point: Point, grid: Vector[Vector[GObj]]): Point = {
    findFurthestMovement(point, grid, Point(0, -1)) 
  }

  def findWesternMostPoint(point: Point, grid: Vector[Vector[GObj]]): Point = {
    findFurthestMovement(point, grid, Point(-1, 0))
  }
  
  def findSouthernMostPoint(point: Point, grid: Vector[Vector[GObj]]): Point = {
    findFurthestMovement(point, grid, Point(0, 1))
  }
  def findEasternMostPoint(point: Point, grid: Vector[Vector[GObj]]): Point = {
    findFurthestMovement(point, grid, Point(1, 0))
  }

  def findFurthestMovement(point: Point, grid: Vector[Vector[GObj]], movement: Point): Point = {
    val nextSpot = Point(point.x + movement.x, point.y + movement.y)
    if (nextSpot.y < 0 || nextSpot.x < 0 || nextSpot.x >= grid(0).length || nextSpot.y >= grid.length) {
      point
    } else if (grid(nextSpot.y)(nextSpot.x) == GObj.Open) {
      findFurthestMovement(nextSpot, grid, movement)
    } else {
      point
    }
  }

  def parseInput(in: List[String]): ParabolicReflectorDish = {
    val parsedGrid = in.map { line => 

      line.map { _ match
        case 'O' => GObj.Round
        case '#' => GObj.Cube
        case '.' => GObj.Open
        case _ => throw new Exception("Should not have any other type of character in parsing")
      }.toVector
    }.toVector

    ParabolicReflectorDish(parsedGrid)
  }
}
