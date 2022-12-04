package com.sandersme.advent.twentyone.model

import com.sandersme.advent.twentyone.graph.Coordinate

import scala.util.matching.Regex

type Cuboids = List[Cuboid]

case class Cuboid(xMin: Int, xMax: Int, yMin: Int, yMax: Int, zMin: Int, zMax: Int,
                  isOn: Boolean = true) {
  def volume: Int = {
    // The +1 is because a min, max 10, 10, 10, 10, 10, 10 is a sized 1 cuboid. 
    val length = xMax - xMin + 1
    val width = yMax - yMin + 1
    val height = zMax - zMin + 1

    length * width * height
  }
}
object Cuboid {

  def intersection(cuboidA: Cuboid, cuboidB: Cuboid): Option[Cuboid] = {
    val maxMinX = Math.min(cuboidA.xMax, cuboidB.xMax)
    val minMaxX = Math.max(cuboidA.xMin, cuboidB.xMin)

    val maxMinY = Math.min(cuboidA.yMax, cuboidB.yMax)
    val minMaxY = Math.max(cuboidA.yMin, cuboidB.yMin)

    val maxMinZ = Math.min(cuboidA.zMax, cuboidB.zMax)
    val minMaxZ = Math.max(cuboidA.zMin, cuboidB.zMin)

    val cuboid = Cuboid(minMaxX, maxMinX, minMaxY, maxMinY, minMaxZ, maxMinZ)

    if (cuboid.xMin> cuboid.xMax || cuboid.yMin > cuboid.yMax || cuboid.zMin > cuboid.zMax)
      None
    else
      Some(cuboid)
  }

  def add(cuboidA: Cuboid, cuboidB: Cuboid): Cuboid = {
    val xMin = Math.min(cuboidA.xMin, cuboidB.xMin)
    val xMax = Math.max(cuboidA.xMax, cuboidB.xMax)
    val yMin = Math.min(cuboidA.yMin, cuboidB.yMin)
    val yMax = Math.max(cuboidA.yMax, cuboidB.yMax)
    val zMin = Math.min(cuboidA.zMin, cuboidB.zMin)
    val zMax = Math.max(cuboidA.zMax, cuboidB.zMax)

    Cuboid(xMin, xMax, yMin, yMax, zMin, zMax)
  }



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
