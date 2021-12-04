package com.sandersme.advent

import junit.framework.TestCase.assertTrue
import munit.Assertions.*
import SonarSweepDayOne.*
import com.sandersme.advent.Accumulator.calculateIncreases

class SonarSweepDayOneTest extends munit.FunSuite  {
  test("Calculate empty list should have 0") {
    val shouldEqualZero = calculateIncreases(List.empty, defaultAccumulator)

    assertEquals(shouldEqualZero.increments, 0)
  }

  test("Calculate all decrement should equal 0") {
    val shouldEqualZero = calculateIncreases(List(5, 4, 3, 2, 1), defaultAccumulator)

    assertEquals(shouldEqualZero.increments, 0)
  }

  test("Calculate increment where every other is above the next. 4 expected increases") {
    val shouldEqualFour = calculateIncreases(List(1, 2, 1, 3, 2, 4, 3, 5), defaultAccumulator)

    assertEquals(shouldEqualFour.increments, 4)
  }

  test("Calculate a list of size one equal 0") {
    val shouldEqualZero = calculateIncreases(List(1), defaultAccumulator)

    assertEquals(shouldEqualZero.increments, 0)
  }

  test("Using the test input from description using previous soundings equal to 7 increases") {
    val input = List(199, 200, 208, 210, 200, 207, 240, 269, 260, 263)
    val shouldEqualSeven = calculateIncreases(input, defaultAccumulator)

    assertEquals(shouldEqualSeven.increments, 7)
  }

  test("Using the test input from using sliding window 3 description equal to 5 increases") {
    val input = List(199, 200, 208, 210, 200, 207, 240, 269, 260, 263)
    val shouldEqualFive = calculateIncreases(input, defaultSlidingWindowAccumulator)

    assertEquals(shouldEqualFive.increments, 5)
  }

  test("Edge case for sliding window not incrementing the last value") {
    val input = List(199, 200, 208, 210, 200, 207, 240, 269, 260, 263, 10, 8, 7, 4, 9, 12)
    val shouldEqualSeven = calculateIncreases(input, defaultSlidingWindowAccumulator)

    assertEquals(shouldEqualSeven.increments, 7)
  }

}
