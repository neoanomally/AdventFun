package com.sandersme.advent.twentyone.model

import com.sandersme.advent.twentyone.graph.Coordinate

import scala.util.matching.Regex

type Cuboids = List[Cuboid]

case class Cuboid(xMin: Int, xMax: Int, yMin: Int, yMax: Int, zMin: Int, zMax: Int, isOn: Boolean)

object Cuboid {

  /**
   * Todo this does not scale :D 
   * @param cuboids
   * @return
   */
  def processCuboids(cuboids: Cuboids): Set[Coordinate] = {
    cuboids
      .foldLeft(Set.empty[Coordinate]){ (activeCubes, cuboid) =>
        val cubes = for {
          x <- cuboid.xMin to cuboid.xMax
          y <- cuboid.yMin to cuboid.yMax
          z <- cuboid.zMin to cuboid.zMax
        } yield Coordinate(x, y, z)

        if (cuboid.isOn) {
          activeCubes ++ cubes
        } else {
          activeCubes -- cubes
        }
      }
  }

  def processCuboidsInPartOne(cuboids: Cuboids): Set[Coordinate] = {
    processCuboids(cuboids)
      .filter(betweenPartOneCoordinates)
  }

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

  def betweenPartOneCoordinates(coordinate: Coordinate): Boolean = {
    coordinate.x >= -50 && coordinate.x <= 50 &&
      coordinate.y >= -50 && coordinate.y <= 50 &&
      coordinate.z >= -50 && coordinate.z <= 50
  }
}
