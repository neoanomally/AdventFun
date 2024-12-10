package com.sandersme.advent.twentyfour

import com.sandersme.advent.Input
import scala.runtime.stdLibPatches.language.`3.0`
import com.sandersme.advent.twentytwo.model.HillClimbing.findAllNeighbors

case class HPoint(x: Int, y: Int) {
  def isValidPoint(height: Int, width: Int): Boolean = {
    x >= 0 && y >= 0 && x < width && y < height
  }

  def generateNeighbors(maxHeight: Int, maxWidth: Int): List[HPoint] = {
    List(
        this.copy(x = x - 1),
        this.copy(y = y - 1),
        this.copy(x = x + 1),
        this.copy(y = y + 1)
      ).filter(p => p.isValidPoint(maxHeight, maxWidth))
  }
}

case class HoofGrid(g: Vector[Vector[Char]]) {
  val height = g.length
  val width = g(0).length

  def valueAt(point: HPoint): Int = {
    if (g(point.y)(point.x).isDigit) {
      g(point.y)(point.x).asDigit
    } else {
      20
    }
  }

  def getStartingPoints: Seq[HPoint] = {
    for { 
      y <- 0 until height
      x <- 0 until width
      
      if (valueAt(HPoint(x, y)) == 0)
    } yield HPoint(x, y)
  }

  def traverseGrid: Int = {
    val startingPoints = getStartingPoints

    val finalStartStop = startingPoints.foldLeft(Set.empty[List[HPoint]]){ case (sum, point) => 
      val current = dfs(point, Set.empty, 0, List.empty, sum)   
      sum ++ current
    }

    finalStartStop.size
  }
 
  def traverseGrid2: Int = {
    val startingPoints = getStartingPoints

    startingPoints.foldLeft(0){ case (sum, point) => 
      val current = dfs2(point, Set.empty, 0)   
      sum + current
    }
  }

  def dfs(point: HPoint, visited: Set[HPoint], expectedHeight: Int, vlist: List[HPoint], trails: Set[List[HPoint]]): Set[List[HPoint]] = {
    if (expectedHeight == 9 && valueAt(point) == 9 ) {
      // println(vlist :+ point)
      trails + List(vlist.head,  point)
    } else if (valueAt(point) == expectedHeight) {
      val updatedVisited = visited + point

      point.generateNeighbors(height, width)
        .filter(neighbor => !updatedVisited.contains(neighbor))
        .map(neighbor => dfs(neighbor, updatedVisited, expectedHeight + 1, vlist :+ point, trails))
        .toSet.flatten
    } else { 
      trails 
    }
  }

  def dfs2(point: HPoint, visited: Set[HPoint], expectedHeight: Int): Int = {
    if (expectedHeight == 9 && valueAt(point) == 9 ) {
      // println(vlist :+ point)
      1
    } else if (valueAt(point) == expectedHeight) {
      val updatedVisited = visited + point

      point.generateNeighbors(height, width)
        .filter(neighbor => !updatedVisited.contains(neighbor))
        .map(neighbor => dfs2(neighbor, updatedVisited, expectedHeight + 1))
        .sum
    } else { 
      0 
    }
  }
}

object  HoofIt {
  def main(args: Array[String]): Unit = {
    val input = Input.readTwentyFourFromResource("day10_input")

    val grid = HoofIt.parseInput(input)
    println("Sum of traversing Grid: " + grid.traverseGrid) 
    println("Sum of traversing all iterations of gid part 2: " + grid.traverseGrid2)
  }

  def parseInput(in: List[String]): HoofGrid = {
    HoofGrid(in
      .map(_.toCharArray.toVector)
      .toVector)
  }
}
