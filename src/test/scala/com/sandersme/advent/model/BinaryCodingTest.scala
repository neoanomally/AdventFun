package com.sandersme.advent.model

import com.sandersme.advent.model.BinaryCoding.*
import com.sandersme.advent.model.Diagnostics.*
import com.sandersme.advent.model.{BinaryCoding, BitTypeCounter}

class BinaryDiagnosticTest extends munit.FunSuite {
  val TEST_INPUT = List(
    "00100",
    "11110",
    "10110",
    "10111",
    "10101",
    "01111",
    "00111",
    "11100",
    "10000",
    "11001",
    "00010",
    "01010"
  )

  val t = Integer.parseInt("1001", 2)

  test("BinaryDiagnostic Parsing that we can parse our binary values into a list of Bits") {
    val parsedValue = BinaryCoding("00100").bits
    val expected = List(Zero, Zero, One, Zero, Zero)

    assertEquals(parsedValue, expected)
  }

  test("Binary Coding converts into a string") {
    val binaryCoding = BinaryCoding(List(Zero, One, One, Zero))
    val binaryCodingString = binaryCoding.toString
    val expectedValue = "0110"

    assertEquals(binaryCodingString, expectedValue)
  }

  test("Binary Coding converts to a number") {
    val binaryCoding = BinaryCoding(List(Zero, One, One, Zero))
    val result = binaryCoding.toInt
    val expectedValue = 6

    assertEquals(result, expectedValue)
  }
}
