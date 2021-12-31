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

}