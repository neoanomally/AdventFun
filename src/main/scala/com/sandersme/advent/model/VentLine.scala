package com.sandersme.advent.model


case class VentLine(pointA: Point, pointB: Point) {
  def isHorizontalLine: Boolean = pointA.x == pointB.x

  def isVerticalLine: Boolean = pointA.y == pointB.y

  def generateAllLinePoints: List[Point] = {
    if (isVerticalLine) {
      generatePoints(pointA.x, pointB.x, pointA.y, true)
    } else if (isHorizontalLine) {
      generatePoints(pointA.y, pointB.y, pointA.x, false)
    } else {
      List.empty
    }
  }

  private def generatePoints(a: Int, b: Int, lineValue: Int, isX: Boolean): List[Point] = {
    val min: Int = Math.min(a, b)
    val max = Math.max(a, b)

    (min to max).map { i =>
      if (isX) Point(i, lineValue) else Point(lineValue, i)
    }.toList
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

  /**
   * For each ventline, generate all points that intersect.
   * Then group by the point type.
   * Get the number of points that had an overlap and count
   * the number of times a point is at an intersection. This is the number of times a point
   * is aggregates to 2 or greater
   * @param ventLines
   * @return
   */
  def countNumberOfOverlappingPoints(ventLines: List[VentLine]): Int = {
    ventLines
      .flatMap(_.generateAllLinePoints)
      .groupBy(identity)
      .map { case (_, points) => points.size }
      .count(_ > 1)
  }
}