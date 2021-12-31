package com.sandersme.advent.math

import com.sandersme.advent.math.VectorDistanceSyntax._

class VectorDistanceTest extends munit.FunSuite {
  test("Test Euclidean distance between two vectors") {
    val v1: Vector[Int] = Vector(1, 1, 0)
    val v2: Vector[Int] = Vector(2, 1, 2)
    val euclideanDistance = VectorDistance.euclideanDistance(v1, v2)

    val diff = Math.abs(euclideanDistance - 2.236)
    assert(diff < 0.01)

    val withSyntax = v1 euclidean v2
    assertEquals(euclideanDistance, withSyntax)
  }

  test("Validate dot product performs the correct calculations") {
    val v1: Vector[Int] = Vector(1, 2, 3)
    val v2: Vector[Int] = Vector(4, -5, 6)

    val dotProduct = VectorDistance.dotProduct(v1, v2)

    assertEquals(dotProduct, 12L)
    assertEquals(dotProduct, v1 dot v2)
  }

  test("Validate that a vector is obtuse to another") {
    val v1: Vector[Int] = Vector(-4, -9)
    val v2: Vector[Int] = Vector(-1, 2)

    val isObtuse = VectorDistance.isObtuse(v1, v2)
    assert(isObtuse)
  }

  test("Validate that a vector is perpendicular relative to another vector") {
    val v1: Vector[Int] = Vector(6, -1, 3)
    val v2: Vector[Int] = Vector(4, 18, -2)

    val isPerpendicular = VectorDistance.isPerpindicular(v1, v2)
    assert(isPerpendicular)
  }

  test("Calculate the magnitude of a vector.") {
    val v: Vector[Int] = Vector(-2, 3)
    val magnitude = VectorDistance.magnitude(v)
    val absApprox = Math.abs(magnitude - 3.6055)

    val v2: Vector[Int] = Vector(3, 5, 2)
    val magnitudeV2 = VectorDistance.magnitude(v2)

    assertEquals(magnitudeV2, Math.sqrt(38.0))
    assert(absApprox < 0.001)
  }

  test("Calculate the cosine distance between two vectors") {
    val v1: Vector[Int] = Vector(3, 4)
    val v2: Vector[Int] = Vector(4, 3)

    val cosine = VectorDistance.cosine(v1, v2)
    val expected = 0.96
    assertEquals(cosine, expected)
  }

  test("test manhattan distances between two 3d vectors") {
    val v1: Vector[Int] = Vector(250, -35, 42)
    val v2: Vector[Int] = Vector(1, 19, -52)

    val manhattan = VectorDistance.manhattanDistance(v1, v2)
    val expected = 397L

    assertEquals(manhattan, expected)
  }
}
