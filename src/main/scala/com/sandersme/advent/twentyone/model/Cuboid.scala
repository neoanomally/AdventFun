package com.sandersme.advent.twentyone.model

import scala.util.matching.Regex

type Cuboids = List[Cuboid]

case class Cuboid(xMin: Int, xMax: Int, yMin: Int, yMax: Int, zMin: Int, zMax: Int, isOn: Boolean)

object Cuboid {
  def parseInput(input: List[String]): Cuboids = {
    input.map{ line =>
      val pattern = new Regex("-?[0-9]+[.][.]-?[0-9]+")

      val allValues = (pattern findAllIn line).toList

      val isOn = line.contains("on")

      val xVals = allValues.head.split("[.][.]")
      val yVals = allValues(1).split("[.][.]")
      val zVals = allValues.last.split("[.][.]")


      Cuboid(xVals.head.toInt, xVals.last.toInt, yVals.head.toInt,
        yVals.last.toInt, zVals.head.toInt, zVals.last.toInt, isOn)
    }
  }
}
