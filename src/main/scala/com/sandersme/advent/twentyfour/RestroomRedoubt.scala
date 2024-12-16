package com.sandersme.advent.twentyfour

import com.sandersme.advent.Input
import scala.collection.mutable

case class RPoint(x: Int, y: Int)
case class Robot(point: RPoint, velocity: RPoint) {
  def move(width: Int, height: Int): Robot = {
    /// TODO Need wrapping logic.  for x > maxX and y > maxY we can use modulo; for <  Max
    val updatedX = moveAndWrap(point.x, velocity.x, width)
    val updatedY = moveAndWrap(point.y, velocity.y, height)
    val updatedPoint = RPoint(updatedX, updatedY)

    this.copy(point = updatedPoint)
  }

  private def moveAndWrap(point: Int, move: Int, max: Int): Int = {
    val update = point + move 
    // println(f"Moving ${point} by ${move} equals: ${point + move}")
    // 11 height, 7 widht
    if (update < 0) {
      max + update
    } else if (update >= max) {
      // println(f" ${update} is above (Max: ${max}) updating to ${update - max} wrapping below zero")
      update - max 
    } else {
      update
    }
  }

}


// TODO: Maybe I need to cache things?
case class BathroomMap(robots: List[Robot], width: Int = 101, height: Int = 103) {
  def moveAllRobots(n: Int): BathroomMap = {
    val updatedRobots = (0 until n).view.foldLeft(robots) { case (updatedRobot, _) =>
      updatedRobot
        .map(_.move(width, height))
    }

    this.copy(robots = updatedRobots)
  }

  def printTiles: Unit = {
    val pointMap = robots.map(_.point)
      .groupMapReduce(identity)(_ => 1)((l, r) => l + r)


    (0 until height).foreach{ y => 
      (0 until width).foreach{ x => 
        pointMap.get(RPoint(x, y)) match {
          case Some(n) => print(n)
          case None => print('.')
        }
      }
      println("")
    }
  }
  
  
  def containsTree: Boolean = {
    val pointMap = robots.map(_.point)
      .groupMapReduce(identity)(_ => 1)((l, r) => l + r)

    val sb = new mutable.StringBuilder
    val results =  (0 until height).map{ y => 
      (0 until width).map{ x => 
        pointMap.get(RPoint(x, y)) match {
          case Some(n) => sb.append('#')
          case None => sb.append('.')
        }
      }
      val res = sb.toString.contains("########")
      sb.clear()
      res
    }
    
    results.exists(identity)
  }


  def printTiles2: Unit = {
    val pointMap = robots.map(_.point)
      .groupMapReduce(identity)(_ => 1)((l, r) => l + r)


    (0 until height).foreach{ y => 
      (0 until width).foreach{ x => 
        pointMap.get(RPoint(x, y)) match {
          case Some(n) => print('#')
          case None => print('.')
        }
      }
      println("")
    }
  }

  def calculateSafetyScore: Int = {
    // Quadrant split by X = 101 / 2   && Y = 103 / 2
    val ignoreX = (width / 2) 
    val ignoreY = (height / 2)

     // x < ignoreX && y < ignoreY -> 1, X > ignoreX && Y < ignore Y, 2
    val results = robots.collect { 
      case robot if (robot.point.x < ignoreX && robot.point.y < ignoreY) => 'a'
      case robot if (robot.point.x > ignoreX && robot.point.y < ignoreY) => 'b'
      case robot if (robot.point.x < ignoreX && robot.point.y > ignoreY) => 'c'
      case robot if (robot.point.x > ignoreX && robot.point.y > ignoreY) => 'd'
    }.groupMapReduce(identity)(_ => 1)(_ + _)

    println("results: " + results)
      
    results.foldLeft(1) { case (sum, (_, next)) => sum * next }
  }
}

object RestroomRedoubt {
  def main(args: Array[String]): Unit = {
    val input = Input.readTwentyFourFromResource("day14_input")
    val bathroomMap = parseInput(input)

    // (0 until 2000).foldLeft(bathroomMap){ case (bathroom, itr) => 
    //
    //   if (itr > 1266) {
    //     println("\n\n\n")
    //     println(f"Iter: ${itr}")
    //     bathroom.printTiles2
        
      //   Thread.sleep(200)
      // }
    findXmasTree(bathroomMap).printTiles2
  }

  def findXmasTree(bathroomMap: BathroomMap, itr: Int = 0): BathroomMap = {
    if (bathroomMap.containsTree || itr > 25000) {
      println("Itr: " + itr)
      bathroomMap
     } else {
      findXmasTree(bathroomMap.moveAllRobots(1), itr + 1)
     }
  }


  // "p=0,4 v=3,-3
  def parseInput(in: List[String]): BathroomMap = {
    val robots = in.map { case line => 
      val split = line.split(" ")

      val p = split(0).substring(2).split(",").map(_.toInt)
      val v = split(1).substring(2).split(",").map(_.toInt)

      Robot(RPoint(p(0), p(1)), RPoint(v(0), v(1)))
    }
    
    BathroomMap(robots)
  }
}
