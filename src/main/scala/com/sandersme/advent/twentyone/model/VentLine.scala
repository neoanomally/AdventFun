package com.sandersme.advent.twentyone.model

import com.sandersme.advent.twentyone.graph.Point

case class VentLine(pointA: Point, pointB: Point) {
  def isDiagonalLine: Boolean = Math.abs(pointA.x - pointB.x) == Math.abs(pointA.y - pointB.y)

  def isHorizontalLine: Boolean = pointA.x == pointB.x

  def isVerticalLine: Boolean = pointA.y == pointB.y

  def generateAllVerticalAndHorizontalPoints: List [Point] = {
    if(isVerticalLine || isHorizontalLine) {
      generatePoints
    } else {
      List.empty
    }
  }

  def generateAllLinePoints: List [Point] = {
    if(isVerticalLine || isHorizontalLine || isDiagonalLine) {
      generatePoints
    } else {
      List.empty
    }
  }

  private def generatePoints: List[Point] = {
    val runDirection = pointA.runDirection(pointB)
    val riseDirection = pointA.riseDirection(pointB)

    val maxSteps = calculateMaxSteps

    (1 to maxSteps).foldLeft(List(pointA)) { (points, _) =>
      val tail = points.last
      points :+ Point(tail.x + runDirection, tail.y + riseDirection)
    }
  }

  private def calculateMaxSteps: Int = {
    val rise = pointA.rise(pointB)
    val run = pointA.run(pointB)

    Math.max(Math.abs(rise), Math.abs(run))
  }
}


object VentLine {
  def parseInput(input: String): VentLine = {
    val split = input.split(" -> ")
    val pointA = Point.parseInput(split(0))
    val pointB = Point.parseInput(split(1))

    VentLine(pointA, pointB)
  }

  def parseInputs(inputs: List[String]): List[VentLine] = {
    inputs.map(VentLine.parseInput)
  }

  def generateRange(start: Int, end: Int): List[Int] = {
    if (start > end) {
      (end to start).reverse.toList
    } else {
      (start to end).toList
    }
  }
  /**
   * For each ventline, generate all points that intersect.
   * Then group by the point type.
   * Get the number of points that had an overlap and count
   * the number of times a point is at an intersection. This is the number of times a point
   * is aggregates to 2 or greater
   * the exec is there just so that we can run the original part 1 code.
   * @param ventLines
   * @return
   */
  def countNumberOfOverlappingPoints(ventLines: List[VentLine],
                                     exec: VentLine => List[Point] =
                                     (ventline) => ventline.generateAllLinePoints): Int = {
    ventLines
      .flatMap(exec)
      .groupBy(identity)
      .map { case (_, points) => points.size }
      .count(_ > 1)
  }

}