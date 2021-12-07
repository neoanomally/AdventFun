package com.sandersme.advent.model

class PointTest extends munit.FunSuite {

  test("Should be able to parse 9,5 to Point") {
    val expectedValue = Point(9, 5)
    val input = "9,5"

    val point = Point.parseInput(input)
    assertEquals(point, expectedValue)
  }

}
