package com.sandersme.advent

import com.sandersme.advent.model.{ BinaryCoding, BitTypeCounter }
import com.sandersme.advent.model.BinaryCoding._
import com.sandersme.advent.model.Diagnostics._

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

  test("Add two bit counters together") {
    val bitA = BitTypeCounter(10, 5)
    val bitB = BitTypeCounter(5, 18)

    val expected = BitTypeCounter(15, 23)
    val result = bitA += bitB

    assertEquals(result, expected)
  }

  test("[Test the sum of two BitCounterLists") {

    val inputTest: List[BinaryCoding] = List(
      BinaryCoding(List(Zero, One, One, Zero, One)),
      BinaryCoding(List(Zero, Zero, One, Zero, One)),
      BinaryCoding(List(One, Zero, Zero, One, Zero))
    )

    val expectedResult: List[BitTypeCounter] = List(
      BitTypeCounter(2, 1), BitTypeCounter(2, 1), BitTypeCounter(1, 2),
      BitTypeCounter(2, 1), BitTypeCounter(1, 2)
    )

    val summedValues = inputTest.toDiagnostics.totalBitCounters
    assertEquals(summedValues, expectedResult)
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

  test("Gamma Rate converted from BitTypeCounter") {
    val diagnostics = TEST_INPUT
      .map(BinaryCoding.apply)
      .toDiagnostics

    val expected = BinaryCoding(List(One, Zero, One, One, Zero))
    val gammaRate = diagnostics.gammaRate

    assertEquals(gammaRate, expected)
  }

  test("Episolon Rate converted from BitTypeCounter") {
    val diagnostics = TEST_INPUT
      .map(BinaryCoding.apply)
      .toDiagnostics

    val expected = BinaryCoding(List(Zero, One, Zero, Zero, One))
    val epsilonRate = diagnostics.epsilonRate

    assertEquals(epsilonRate, expected)
  }

  test("End to end test with test input case for power consumption") {
    val diagnostics = TEST_INPUT
      .map(BinaryCoding.apply)
      .toDiagnostics

    val gammRate = diagnostics.gammaRate.toInt
    val epsilonRate = diagnostics.epsilonRate.toInt
    val powerConsumption = diagnostics.powerConsumption

    val expectedGammarate = 22
    val expectedEpsilonRate = 9
    val expectedPowerRate = 198

    assertEquals(epsilonRate, expectedEpsilonRate)
    assertEquals(gammRate, expectedGammarate)
    assertEquals(powerConsumption, expectedPowerRate)
  }

  test("Calculate the Oxygen Generator Rating based on TEST_INPUT expected output 23") {
    val expectedOxygenRating = 23

    assert(false)
  }

  test("Calculate the CO2 Scrubber Rating from TEST_INPUT expected output is 10") {
    val expectedCO2Rating = 10

    assert(false)
  }

  test("Calculate the life support rating from TEST_INPUT which should be 23 * 10 or 23") {
    val expectedLifeSupportRating = 230

    assert(false)
  }
}
