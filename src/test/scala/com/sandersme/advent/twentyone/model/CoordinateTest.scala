package com.sandersme.advent.twentyone.model

import com.sandersme.advent.twentyone.graph.Coordinate

class CoordinateTest extends munit.FunSuite {
  test("Cross product between two coordinates") {
    val a = Coordinate(3, -5, 4)
    val b = Coordinate(2, 6, 5)

    val c = a crossProduct b

    assertEquals(c, Coordinate(-49, -7, 28))
  }

}
