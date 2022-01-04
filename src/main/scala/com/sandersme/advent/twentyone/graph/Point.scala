package com.sandersme.advent.twentyone.graph

case class Point(x: Int, y: Int) {
  override def toString: String = s"($x,$y)"

  def distance(otherPoint: Point): Double = {
    val xSquared = Math.pow(otherPoint.x - x, 2.0)
    val ySquared = Math.pow(otherPoint.y - y, 2.0)

    Math.sqrt(xSquared + ySquared)
  }

  def rise(otherPoint: Point): Int = otherPoint.y -  y

  def run(otherPoint: Point): Int = otherPoint.x - x

  def runDirection(otherPoint: Point): Int = getStepDirection(run(otherPoint))
  def riseDirection(otherPoint: Point): Int = getStepDirection(rise(otherPoint))

  def slope(otherPoint: Point): Double = rise(otherPoint).toDouble / run(otherPoint).toDouble

  private def getStepDirection(pointDirection: Int): Int = {
    if (pointDirection < 0) {
      -1
    } else if (pointDirection > 0) {
      1
    } else {
      0
    }
  }
}

object Point {
  def parseInput(input: String): Point = {
    val split = input.split(",")

    val x = split(0).toInt
    val y = split(1).toInt

    Point(x, y)
  }
}