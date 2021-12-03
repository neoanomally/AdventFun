package com.sandersme.advent

import junit.framework.TestCase.assertTrue
import munit.Assertions.*
import SonarSweepDayOne._

class SonarSweepDayOneTest extends munit.FunSuite  {
  test("Calculate empty list should have 0") {
    val shouldEqualZero: Accumulator = calculateNumberOfIncreases(List.empty)

    assertEquals(shouldEqualZero.sumOfIncreases, 0)
  }

  test("Calculate all decrement should equal 0") {
    val shouldEqualZero = calculateNumberOfIncreases(List(5, 4, 3, 2, 1))

    assertEquals(shouldEqualZero.sumOfIncreases, 0)
  }

  test("Calculate increment where every other is above the next. 4 expected increases") {
    val shouldEqualFour = calculateNumberOfIncreases(List(1, 2, 1, 3, 2, 4, 3, 5))

    assertEquals(shouldEqualFour.sumOfIncreases, 4)
  }

  test("Calculate a list of size one equal 0") {
    val shouldEqualZero = calculateNumberOfIncreases(List(1))

    assertEquals(shouldEqualZero.sumOfIncreases, 0)
  }

  test("Using the test input from description equal to 7 increases") {
    val input = List(199, 200, 208, 210, 200, 207, 240, 269, 260, 263)
    val shouldEqualSeven = calculateNumberOfIncreases(input)

    assertEquals(shouldEqualSeven.sumOfIncreases, 7)
  }

}
