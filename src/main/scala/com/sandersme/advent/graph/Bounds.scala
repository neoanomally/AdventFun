package com.sandersme.advent.graph


type TargetBounds = Bounds
type ProbeBounds = Bounds

case class Bounds(minX: Int, maxX: Int, minY: Int, maxY:Int) {
  /**
   * The following two methods are to check to see if X
   * has made it within bounds AND the second one to see if
   * it has overshot boundary.
   *
   * @param x (Current X Location)
   * @return True if in bounds
   */
  def isXInBounds(x: Int): Boolean = minX <= x && x <= maxX
  def isXPassedBoundry(x: Int): Boolean = x > maxX

  /**
   * The following methods are to check to see if y has made it within
   * Bounds or has overshot the boundary.  Similar to the one above
   *
   * @param y
   * @return
   */
  def isYInBounds(y: Int): Boolean = minY <= y && y <= maxY
  def isYBeyondBoundry(y: Int): Boolean = y < minY

  def isBeyondBounds(x: Int, y: Int): Boolean = {
    isXPassedBoundry(x) || isYBeyondBoundry(y)
  }

  def isInBounds(x: Int, y: Int): Boolean = {
    isXInBounds(x) && isYInBounds(y)
  }
}

object Bounds {
  def targetBoundsFromInput(input: String): Bounds = {
    val cleaned = input.drop(13)
    val split = cleaned.split(", ")
    val (minX: Int, maxX: Int) = parseMinMax(split.head)
    val (minY: Int, maxY: Int) = parseMinMax(split.last)

    Bounds(minX, maxX, minY, maxY)
  }

  private def parseMinMax(cleaned: String): (Int, Int) = {
    val splitInt = cleaned.drop(2).split("\\.\\.").map(_.toInt)

    (splitInt.head, splitInt.last)
  }
}