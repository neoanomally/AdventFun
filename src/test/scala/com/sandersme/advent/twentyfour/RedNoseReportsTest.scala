package com.sandersme.advent.twentyfour

import com.sandersme.advent.twentyfour.RedNoseReports
import munit.FunSuite
import com.sandersme.advent.twentyfour.RedNoseReports.isLineSafe

class RedNoseReportsTest extends munit.FunSuite {
  test("Test that I can parse through the input") { 
    val parsedLine = RedNoseReports.parseLine(TEST_INPUT.head)

    val expectedLine = List(7, 6, 4, 2, 1)

    assertEquals(parsedLine, expectedLine)
  }

  test("Test edge cases where the lines should be replaced") {
    val allEqual = List(7, 7, 7, 7, 7)
    val ascendDescend = List(4, 5, 4, 3, 2, 1)
    val oppositeAllAscend = List(1, 2, 3, 4, 5, 4)
    val twoStarting = List(4, 4, 3, 2, 1)
    val twoEnding = List(4, 3, 2, 1, 1)
    val realTestCase = List(1, 3,5, 6, 8, 9, 12, 9)
    val anotherRealCase = List(1, 1, 2, 3, 4, 5, 6)
    val oneOfFour = List(25, 23, 25, 28, 29, 32, 35)
    val twoOfFour = List(8, 11, 9, 7, 4)
    val threeOfFour = List(44, 41, 42, 44, 46, 47, 48, 5)
    val fourOfFour = List(48, 45, 46, 49, 52)

    assertEquals(isLineSafe(allEqual, isIgnored = true), false)
    assertEquals(isLineSafe(ascendDescend, isIgnored = true), false)
    assertEquals(isLineSafe(oppositeAllAscend, isIgnored = true), true)
    assertEquals(isLineSafe(twoStarting, isIgnored = true), true)
    assertEquals(isLineSafe(twoEnding, isIgnored = true), true)
    assertEquals(isLineSafe(realTestCase, isIgnored = true), true)
    assertEquals(isLineSafe(anotherRealCase, isIgnored = true), true)
    assertEquals(isLineSafe(oneOfFour, isIgnored = true), false)
    assertEquals(isLineSafe(twoOfFour, isIgnored = true), false)
    assertEquals(isLineSafe(threeOfFour, isIgnored = true), false)
    assertEquals(isLineSafe(fourOfFour, isIgnored = true), false)
  }

  test("Test whether or not line is valid for part one") {
    val isSafe = PARSED_INPUT.map(line => RedNoseReports.isLineSafe(line, isIgnored = false)) 

    val expected = List(true, false, false, false, false, true)
    assertEquals(isSafe, expected)
  }
  
  test("Test that num lines are safe with removal") {
    val isSafe = PARSED_INPUT.map(line => RedNoseReports.isLineSafe(line, isIgnored = true))

    val expected = List(true, false, false, true, true, true)
  }

  test("Test first line is safe should be true") {
    val isLineSafe = RedNoseReports.isLineSafe(PARSED_INPUT.head)

    val expected = true

    assertEquals(isLineSafe, expected)
  }

  test("Test a single two values is safe") {
    val first = RedNoseReports.isSafe(5, 6, false)
    val second = RedNoseReports.isSafe(6, 4, true)
    val third = RedNoseReports.isSafe(5, 6, true)
    val fourth = RedNoseReports.isSafe(1, 5, true)
    val fifth = RedNoseReports.isSafe(4, 4, false)
    val sixth = RedNoseReports.isSafe(4, 4, true)

    val expectedFirst = true
    val expectedSecond = true
    val expectedThird = false
    val expectedFourth = false
    val expectedFifth = false
    val expectedSixth = false

    assertEquals(first, expectedFirst)
    assertEquals(second, expectedSecond)
    assertEquals(third, expectedThird)
    assertEquals(fourth, expectedFourth)
    assertEquals(fifth, expectedFifth)
    assertEquals(sixth, expectedSixth)
  }

  val TEST_INPUT = """7 6 4 2 1
    1 2 7 8 9
    9 7 6 2 1
    1 3 2 4 5
    8 6 4 4 
    1 3 6 7 9"""
    .stripMargin
    .split("\n")
    .toList
    .map(_.trim)

  val PARSED_INPUT = TEST_INPUT.map(RedNoseReports.parseLine)
}
