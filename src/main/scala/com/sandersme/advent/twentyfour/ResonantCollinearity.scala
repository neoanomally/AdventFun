package com.sandersme.advent.twentyfour

import com.sandersme.advent.Input
import com.sandersme.advent.twentyfour.ResonantPoint
import com.sandersme.advent.twentyfour.SlopeType

case class ResonantCollinearity(width: Int, height: Int, pointMap: Map[Char, List[ResonantPoint]]) {
  lazy val pointSet = pointMap.values.flatten.toSet

  def calculateAllAntiNodes: Int = {
    val f = pointMap.flatMap{ (_, nodes) => 
      for {
        left <- 0 until nodes.length
        right <- left + 1 until nodes.length

        antiNode <- ResonantCollinearity.calculateAntiNodes(nodes(left), nodes(right))

        if (antiNode.x >= 0 && antiNode.y >= 0 && antiNode.x < width && antiNode.y < height)
      } yield antiNode
    }.toList
    
    // println("BEFORE SET TRANSFORMATION:")
    // f.sortBy(_.x).foreach(println)
    // println("\n\nAFTER SET TRANSFORMATION")
    // f.toSet.toList.sortBy(_.x).foreach(println)

    f.toSet.size
  }

  def calculateAllResoundingAntiNodes: Int = {
    val f = pointMap.flatMap{ (_, nodes) => 
      for {
        left <- 0 until nodes.length
        right <- left + 1 until nodes.length

        antiNode <- ResonantCollinearity.calculateResoundingAntiNodes(nodes(left), nodes(right),height, width)

        if (antiNode.x >= 0 && antiNode.y >= 0 && antiNode.x < width && antiNode.y < height)
      } yield antiNode
    }.toList


    f.toSet.size
  }
}

case class ResonantPoint(x: Int, y: Int) {
  def add(otherX: Int, otherY: Int): ResonantPoint = {
    ResonantPoint(this.x + otherX, this.y + otherY)
  }
}


sealed trait SlopeType
case object Horizontal extends SlopeType
case object Vertical extends SlopeType
case object PosDiagnal extends SlopeType
case object NegDiagnal extends SlopeType

object ResonantCollinearity {
  def main(args: Array[String]): Unit = {

    val input = Input.readTwentyFourFromResource("day8_input")
    val parsedInput = ResonantCollinearity.parseInput(input)

    println("The number of antinodes created: " + parsedInput.calculateAllAntiNodes)

    println("The number of resounding AntiNodes created: " + parsedInput.calculateAllResoundingAntiNodes)
  }

  def calculateAntiNodes(pointA: ResonantPoint, pointB: ResonantPoint): List[ResonantPoint] = {
    val (left, right) = if (pointA.x == pointB.x && pointA.y < pointB.y) {
      (pointA, pointB)
    } else if (pointA.x < pointB.x) { 
      (pointA, pointB) 
    } else { 
      (pointB, pointA) 
    }
    val (rise, run) = calculateRiseRun(left, right)

    List(left.add(run, rise), right.add(-run, -rise))
  }


  def calculateResoundingAntiNodes(pointA: ResonantPoint, pointB: ResonantPoint, height: Int, width: Int): List[ResonantPoint] = {
    val (left, right) = if (pointA.x == pointB.x && pointA.y < pointB.y) {
      (pointA, pointB)
    } else if (pointA.x < pointB.x) { 
      (pointA, pointB) 
    } else { 
      (pointB, pointA) 
    }
    val (rise, run) = calculateRiseRun(left, right)

    addUntilOutOfBounds(run, rise, height, width, List(left)) ++ addUntilOutOfBounds(-run, -rise, height, width, List(right))
  }

  def addUntilOutOfBounds(run: Int, rise: Int, height: Int, width: Int, accum: List[ResonantPoint]): List[ResonantPoint]  = {
    val last = accum.last
    val newPoint = last.add(run, rise)

    if (newPoint.x >= 0 && newPoint.y >= 0 && newPoint.x < width && newPoint.y < height) {
      addUntilOutOfBounds(run, rise, height, width, accum :+ newPoint)
    } else {
      accum :+ newPoint
    }
  }

  // TODO: Probably should just return a Slope(rise: Int, run: Int) object
  def calculateRiseRun(left: ResonantPoint, right: ResonantPoint): (Int, Int) = {
    val run = left.x - right.x
    val rise = left.y - right.y

    (rise, run)
  }

  def calcSlopeType(rise: Int, run: Int): SlopeType = {
      
    if (rise == 0) {
      Horizontal
    } else if (run == 0) {
      Vertical
    } else if(rise == run) {
      NegDiagnal
    } else {
      PosDiagnal
    }
  }

  def calculateDistance(left: ResonantPoint, right: ResonantPoint): Int = {
    Math.max(Math.abs(left.y - right.y), Math.abs(left.x - right.x))
  }

  def isStraightLine(left: ResonantPoint, right: ResonantPoint): Boolean = {
    val slope = calculateSlope(left, right)
    Math.abs(slope) == 1.0 || slope == 0.0
  }

  def calculateSlope(left: ResonantPoint, right: ResonantPoint): Double = {
    (right.y - left.y).toDouble / (right.x - left.x)
  }

  def parseInput(in: List[String]): ResonantCollinearity = {
    val width = in(0).length 
    val height = in.length

    val points = for { 
      y <- 0 until height
      x <- 0 until width

      c = in(y).charAt(x)
      
      if (c != '.')
    } yield (c, ResonantPoint(x, y))


    val pointsMap = points
      .groupBy(_._1)
      .map((k, v) => k -> v.map(_._2).toList)

      ResonantCollinearity(width, height, pointsMap)
  }
}
