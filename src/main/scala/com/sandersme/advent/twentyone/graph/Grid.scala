package com.sandersme.advent.twentyone.graph

case class Grid()

object Grid {

  def generateNeighborsMinusDiagnonals(x: Int, y: Int, maxX: Int, maxY: Int): List[Point] = {
      List(
        Point(x, y + 1),
        Point(x, y - 1),
        Point(x + 1, y),
        Point(x - 1, y)
      ).filter(point => point.x >= 0 && point.y >= 0)
        .filter(point => point.x < maxX && point.y < maxY)
  }

  def generateNeighborValues(x: Int, y: Int, maxX: Int, maxY: Int): List[Point] = {
    val results = for {
      xVal <- (-1 to 1).map(_ + x)
      yVal <- (-1 to 1).map(_ + y)

      if(xVal >= 0 && yVal >= 0) &&
        (xVal < maxX && yVal < maxY) &&
        (xVal != x || y != yVal)
    }  yield Point(xVal, yVal)

    results.toList
  }
}
