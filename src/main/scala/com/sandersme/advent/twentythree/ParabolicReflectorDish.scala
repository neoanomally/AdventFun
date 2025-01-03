package com.sandersme.advent.twentythree

import com.sandersme.advent.twentyfour.Point
import com.sandersme.advent.twentythree.ParabolicReflectorDish.findNorthernMostPoint
import com.sandersme.advent.twentythree.ParabolicReflectorDish.moveRock
import com.sandersme.advent.Input


enum GObj { 
  case Cube, Round, Open
}
case class ParabolicReflectorDish(grid: Vector[Vector[GObj]]) {
  val height = grid.length
  val width = grid(0).length

  def objAt(point: Point): GObj = {
    grid(point.y)(point.x)
  }

  // Right now it's just north but I imagine we'll tilt several times
  def tiltRocksNorth: ParabolicReflectorDish = {
    val updatedGrid = (0 until height).foldLeft(grid){ case (yGrid, y) =>
      (0 until width).foldLeft(yGrid) { case (xGrid, x) =>
        if (xGrid(y)(x) == GObj.Round) {
          val northerOpenPoint = findNorthernMostPoint(Point(x, y), xGrid, Point(x, y))
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
  }

  def moveRock(current: Point, newPoint: Point, grid: Vector[Vector[GObj]]): Vector[Vector[GObj]] = {
    val objectAtCurrent = grid(current.y)(current.x)
    val emptyCurrent = grid.updated(current.y, grid(current.y).updated(current.x, GObj.Open))
    
    emptyCurrent.updated(newPoint.y, emptyCurrent(newPoint.y).updated(newPoint.x, objectAtCurrent))
  }

  def findNorthernMostPoint(point: Point, grid: Vector[Vector[GObj]], movement: Point): Point = {
    val nextSpot = point.copy(y = point.y - 1)
    if (nextSpot.y < 0) {
      point
    } else if (grid(nextSpot.y)(nextSpot.x) == GObj.Open) {
      findNorthernMostPoint(nextSpot, grid, movement)
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
