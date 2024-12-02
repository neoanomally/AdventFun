package com.sandersme.advent.twentyfour

import com.sandersme.advent.twentyfour.RedNoseReports
import munit.FunSuite

class RedNoseReportsTest extends munit.FunSuite {
  test("Test that I can parse through the input") { 
    val parsedLine = RedNoseReports.parseLine(TEST_INPUT.head)

    val expectedLine = List(7, 6, 4, 2, 1)

    assertEquals(parsedLine, expectedLine)
  }

  test("Test whether or not line is valid") {
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

    val expectedFirst = true
    val expectedSecond = true
    val expectedThird = false
    val expectedFourth = false

    assertEquals(first, expectedFirst)
    assertEquals(second, expectedSecond)
    assertEquals(third, expectedThird)
    assertEquals(fourth, expectedFourth)
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
