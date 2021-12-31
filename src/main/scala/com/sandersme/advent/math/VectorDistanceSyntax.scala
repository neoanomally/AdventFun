package com.sandersme.advent.math

import scala.math.Numeric.Implicits._

object VectorDistanceSyntax {
  extension[T: Numeric] (thisVector: Vector[T])
    def dot(thatVector: Vector[T]): Long =
      VectorDistance.dotProduct(thisVector, thatVector)

    def dotNorm(thatVector: Vector[T]): Long =
      VectorDistance.normDotProduct(thisVector, thatVector)

    def euclidean(thatVector: Vector[T]): Double =
      VectorDistance.euclideanDistance(thisVector, thatVector)

    def manhattan(thatVector: Vector[T]): Long = {
      VectorDistance.manhattanDistance(thisVector, thatVector)
    }

    def magnitude: Double =
      VectorDistance.magnitude(thisVector)

    def cosine(thatVector: Vector[T]): Double =
      VectorDistance.cosine(thisVector, thatVector)
}
