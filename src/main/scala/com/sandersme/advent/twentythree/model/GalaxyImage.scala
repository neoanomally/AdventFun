package com.sandersme.advent.twentythree.model

import com.sandersme.advent.twentyone.graph.Point


case class GalaxyImage(galaxyPoints: Vector[Point]) { 
  /**
      * One thing I dont' like about this is that I'm iterating over the vector 4 times,
      * but it is simpler than creating a Custom accumulator
      *
      * @return
      */
  def expandGalaxy(factor: Int = 2): GalaxyImage = {
    val distinctX = galaxyPoints.map(_.x).toSet
    val distinctY = galaxyPoints.map(_.y).toSet

    val emptyXs = sortedEmptyValues(distinctX)
    val emptyYs = sortedEmptyValues(distinctY)

    val updatedPoints = galaxyPoints.map { case point =>
      val moveX = emptyXs.count(_ < point.x) * (factor - 1)
      val moveY = emptyYs.count(_ < point.y) * (factor - 1)

      Point(point.x + moveX, point.y + moveY)
    }

    GalaxyImage(updatedPoints)
  }

  // If need to in the future we may need to search for the "Range" of values instead of
  // materializing each value. 
  private[this] def sortedEmptyValues(distinctPosition: Set[Int]): List[Int] = {
    val results = for {
      value <- 0 to distinctPosition.max
      if !distinctPosition.contains(value)
    } yield value

    results.toList.sorted
  }

  def findEveryPointsPair: List[(Point, Point)] = {
    val pairs = for {
      left <- 0 to galaxyPoints.size - 2
      right <- left + 1 to galaxyPoints.size - 1
    } yield (galaxyPoints(left), galaxyPoints(right))

    pairs.toList
  }

  def sumDistanceEveryPair: Long = {
    findEveryPointsPair
      .map((l, r) => GalaxyImage.calculateDistanceBetweenPoints(l, r))
      .sum
  }
}

object GalaxyImage {
  def parseInput(input: List[String]): GalaxyImage = {
    val galaxyVector = input.zipWithIndex.flatMap((v, y) => v.zipWithIndex.map { case (chara, x) =>
      chara match {
        case '#' => Some(Point(x, y))
        case _   => None
      }
    }).flatten
    .toVector

    GalaxyImage(galaxyVector)
  }
  
  def calculateDistanceBetweenPoints(left: Point, right: Point): Long = {
    val absX = Math.abs(left.x - right.x)
    val absY = Math.abs(left.y - right.y)
    
    absY + absX
  }
}
