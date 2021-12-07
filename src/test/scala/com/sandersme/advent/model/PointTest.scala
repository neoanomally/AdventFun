package com.sandersme.advent.model

class PointTest extends munit.FunSuite {

  test("Should be able to parse 9,5 to Point") {
    val expectedValue = Point(9, 5)
    val input = "9,5"

    val point = Point.parseInput(input)
    assertEquals(point, expectedValue)
  }

  test("Check the distance between two points") {
    val expectedDistance = 5.656854

    val pointA = Point(9, 5)
    val pointB = Point(5, 9)

    val distance = pointA.distance(pointB)
    assertEqualsDouble(distance, expectedDistance, 0.01)
  }

  test("Assert Run for line 9, 5 and 5, 9 is -4") {
    val thisPoint = Point(9, 5)
    val thatPoint = Point(5, 9)

    val expectedRun = -4
    val run = thisPoint.run(thatPoint)

    assertEquals(run, expectedRun)
  }

  test("Assert Rise for line 9, 5 and 5, 9  is +4") {
    val thisPoint = Point(9, 5)
    val thatPoint = Point(5, 9)

    val expectedRise = 4
    val rise = thisPoint.rise(thatPoint)

    assertEquals(rise, expectedRise)
  }

  test("Assert slope is -1 for line on points 9, 5 and 5,9 is -1") {
    val thisPoint = Point(9, 5)
    val thatPoint = Point(5, 9)

    val expectedSlope: Double = -1
    val slope = thisPoint.slope(thatPoint)

    assertEqualsDouble(slope, expectedSlope, 0.01)
  }

  test("Make sure that we get the right rise and run directions for (9,5) (5,9)") {
    val thisPoint = Point(9, 5)
    val thatPoint = Point(5, 9)

    val riseDirection = thisPoint.riseDirection(thatPoint)
    val runDirection = thisPoint.runDirection(thatPoint)
    val expectedRiseDirection = 1
    val expectedRunDirection = -1

    val riseZeroDirection = thisPoint.riseDirection(thisPoint)
    val expectedZero = 0

    assertEquals(riseZeroDirection, expectedZero)
    assertEquals(riseDirection, expectedRiseDirection)
    assertEquals(runDirection, expectedRunDirection)
  }
}
