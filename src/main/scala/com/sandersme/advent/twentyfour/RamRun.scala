package com.sandersme.advent.twentyfour

import com.sandersme.advent.twentyfour.Point
import com.sandersme.advent.twentyfour.RamRun.isInBounds
import com.sandersme.advent.Input
import scala.collection.immutable.Queue

case class RamRun(currentLoc: Point, bytes: Vector[Point], droppedBytes: Set[Point], dims: (Int, Int), debug: Boolean = false ) {
  def width:  Int = dims._1
  def height: Int = dims._2 

  val endPoint = Point(width - 1, height - 1)
  def isInBound: Boolean = isInBounds(dims, currentLoc)
  
  def advanceState: RamRun = {
    ???
  }

  def findMinNumberSteps: Int = {
    bfs(Set(Point(0, 0)), Set.empty)
  }

  def findUnvisitedNeighbors(visited: Set[Point], point: Point): List[Point] = {
    List(
      Point(point.x - 1, point.y),
      Point(point.x + 1, point.y),
      Point(point.x, point.y - 1),
      Point(point.x, point.y + 1),
    )
      .filter(p => isInBounds(dims, point))
      .filter(p => !visited.contains(p))
      .filter(p => !droppedBytes.contains(p))
  }

  def bfs(queue: Set[Point], visited: Set[Point], steps: Int = -1): Int= {
    if(debug) {
      printGrid(visited)
    }
    if (visited.contains(endPoint) || queue.isEmpty) {
      steps
    } else {
      val updatedVisited = visited ++ queue

      val updatedQueue = queue.flatMap(point => findUnvisitedNeighbors(updatedVisited, point).toSet)
      if (debug) {
        println("Visiting: " + updatedQueue)
      }
      bfs(updatedQueue, updatedVisited, steps + 1)
    }
  }

  def startCanMakeIt: Boolean = {
    bfsCanMakeIt(Set(Point(0, 0)), Set.empty) 
  } 

  def bfsCanMakeIt(queue: Set[Point], visited: Set[Point]): Boolean = {
    if (visited.contains(endPoint)) {
      true
    } else if (queue.isEmpty) {
      false
    } else {
      val updatedVisited = visited ++ queue

      val updatedQueue = queue.flatMap(point => findUnvisitedNeighbors(updatedVisited, point).toSet)
      bfsCanMakeIt(updatedQueue, updatedVisited)
    }
  }

  def advanceBytes(n: Int): RamRun = {
    val nextNBytes = bytes.take(n)
    val remaining = bytes.drop(n)
    val updatedDropped = droppedBytes ++ nextNBytes

    this.copy(bytes = remaining, droppedBytes = updatedDropped)
  }

  def printGrid(visited: Set[Point]): Unit = {
    println("\n\n")
    (0 until height).foreach { y =>
      (0 until width).foreach { x => 
        if (droppedBytes.contains(Point(x, y))) {
          print("#")
        } else if (visited.contains(Point(x, y))) {
          print("O")
        } else {
          print(".")
        }
      }
      println("")
    }
  }
}


/// One thing I can do is try to figure out if I can get past the bytes at time T
// so if I step on a space before the last byte. I also need to figure out
// Where the last byte is by thte time it's up. 
//
object RamRun {

  // extension (ramRun: RamRun) {
  // }

  def isInBounds(dims: (Int, Int), point: Point): Boolean = {
    point.x >= 0 && point.y >= 0 && point.x < dims._1 && point.y < dims._2
  }


  def binarySearchFirstPoint(ramRun: RamRun): Point = {
    def binarySearch(left: Int, right: Int): Int = {
      if (ramRun.debug) {
        println(f"Left: $left  Right: $right")
      }
      if (left >= right) {
        if (ramRun.debug) {  println(f"FOUND: ${left} or ${right}") }
        left
      } else {
        val mid = left + ((right - left) / 2)

        if (ramRun.debug) {
          println("Mid: " + mid)
        }
        val canMakeIt = ramRun.advanceBytes(mid)
          .startCanMakeIt
      
        if (ramRun.debug) { println("Checking Mid: " + mid + " can make it: " + canMakeIt) }

        if (canMakeIt) {
          binarySearch(mid + 1, right)
        } else {
          binarySearch(left, mid - 1)
        }
      }
    }

    val idx = binarySearch(0, ramRun.bytes.size - 1)

    ramRun.bytes(idx)
  }


  def main(args: Array[String]): Unit = {

    val input = Input.readTwentyFourFromResource("day18_input")
    
    val ramRun = parseInput(input, 71, 71) 
    val minSteps = ramRun
      .advanceBytes(1024)
      .findMinNumberSteps

    println("The min number of steps for part 1: " + minSteps)

    val firstPointBlcoksPath = binarySearchFirstPoint(ramRun)

    println("The first point that blocks a path is: " + firstPointBlcoksPath)
  }

  def parseInput(in: List[String], width: Int, height: Int): RamRun = {
    val points = in.map(_.split(",").map(_.toInt))
      .map(split => Point(split(0), split(1)))
      .toVector


    RamRun(Point(0, 0), points, Set.empty, (width, height))
  }
}
