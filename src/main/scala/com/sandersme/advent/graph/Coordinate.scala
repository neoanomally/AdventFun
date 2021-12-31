package com.sandersme.advent.graph

import com.sandersme.advent.math.VectorDistance

case class Coordinate(x: Int, y: Int, z: Int) {
  override def toString: String = s"($x, $y, $z)"

  def asVector: Vector[Int] = Vector(x, y, z)

}

object Coordinate {
  extension (thisCoord: Coordinate)
    def manhattan(thatCoord: Coordinate): Long = {
      VectorDistance.manhattanDistance(thisCoord.asVector, thatCoord.asVector)
    }


    def crossProduct(thatCoord: Coordinate): Coordinate = {
      val x = (thisCoord.y * thatCoord.z) - (thisCoord.z * thatCoord.y)
      val y = (thisCoord.z * thatCoord.x) - (thisCoord.x * thatCoord.z)
      val z = (thisCoord.x * thatCoord.y) - (thisCoord.y * thatCoord.x)

      Coordinate(x, y, z)
    }
}