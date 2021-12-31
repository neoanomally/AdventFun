package com.sandersme.advent.math

import scala.math.Numeric.Implicits._

object VectorDistance {
  def euclideanDistance[T : Numeric](v1: Vector[T], v2: Vector[T]): Double = {
    require(v1.length == v2.length, "ERROR Vecor lengths must be equivalent")
    val sumOfSquares = (v1 zip v2)
      .map(pair => Math.pow(pair._1.toDouble - pair._2.toDouble, 2))
      .sum

    Math.sqrt(sumOfSquares)
  }

  def manhattanDistance[T: Numeric](v1: Vector[T], v2: Vector[T]): Long = {
    require(v1.length == v2.length, "ERROR Vecor lengths must be equivalent")

    (v1 zip v2).map(pair =>
      Math.abs(pair._1.toLong - pair._2.toLong)
    ).sum
  }

  def dotProduct[T: Numeric](v1: Vector[T], v2: Vector[T]): Long = {
    require(v1.length == v2.length, "ERROR Vecor lengths must be equivalent")

    val sumOfProduct: Long = (v1 zip v2)
      .map(pair => pair._1.toLong * pair._2.toLong )
      .sum

    sumOfProduct
  }

  def magnitude[T: Numeric](vector: Vector[T]): Double = {
    Math.sqrt(vector.map(v => Math.pow(v.toDouble, 2.0)).sum)
  }

  def normDotProduct[T: Numeric] (v1: Vector[T], v2: Vector[T]): Long = {
    require(v1.length == v2.length, "ERROR Vecor lengths must be equivalent")

    val dot = dotProduct(v1, v2)
    dot / v1.length
  }

  def cosine[T: Numeric](v1: Vector[T], v2: Vector[T]): Double = {
    val dotPrudct = dotProduct(v1, v2)

    val v1Magnitude = magnitude(v1)
    val v2Magnitude = magnitude(v2)

    val magnitudeProduct = v1Magnitude * v2Magnitude

    dotPrudct / magnitudeProduct
  }

  def isPerpindicular[T: Numeric](v1: Vector[T], v2: Vector[T]): Boolean = {
    dotProduct(v1, v2) == 0L
  }

  def isObtuse[T: Numeric](v1: Vector[T], v2: Vector[T]): Boolean = {
    dotProduct(v1, v2) < 0L
  }

  def isAcute[T: Numeric](v1: Vector[T], v2: Vector[T]): Boolean = {
    dotProduct(v1, v2) > 0L
  }
}
