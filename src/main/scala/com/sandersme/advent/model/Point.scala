package com.sandersme.advent.model

case class Point(x: Int, y: Int)

object Point {
  def parseInput(input: String): Point = {
    val split = input.split(",")

    val x = split(0).toInt
    val y = split(1).toInt

    Point(x, y)
  }
}