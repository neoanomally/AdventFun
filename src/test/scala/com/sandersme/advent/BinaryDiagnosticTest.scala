package com.sandersme.advent

import com.sandersme.advent.BinaryDiagnostic.*
import com.sandersme.advent.BinaryDiagnostic.BinaryCoding.*

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
    val parsedValue = BinaryDiagnostic.BinaryCoding("00100").bits
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
    import com.sandersme.advent.BinaryDiagnostic.BinaryCoding._

    val inputTest: List[BinaryCoding] = List(
      BinaryCoding(List(Zero, One, One, Zero, One)),
      BinaryCoding(List(Zero, Zero, One, Zero, One)),
      BinaryCoding(List(One, Zero, Zero, One, Zero))
    )

    val expectedResult: BitTypeCounters = BitTypeCounters( List(
      BitTypeCounter(2, 1), BitTypeCounter(2, 1), BitTypeCounter(1, 2),
      BitTypeCounter(2, 1), BitTypeCounter(1, 2)
    ))

    val summedValues = inputTest.sumBinaryCodingColumns
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
    val bitTypeCounters: BitTypeCounters = BitTypeCounters(List(
      BitTypeCounter(5, 3),
      BitTypeCounter(6, 2),
      BitTypeCounter(3, 5),
      BitTypeCounter(2, 8),
      BitTypeCounter(2, 2)
    ))

    val expected = BinaryCoding(List(Zero, Zero, One, One, One))
    val gammaRate = bitTypeCounters.gammaRate

    assertEquals(gammaRate, expected)
  }

  test("Episolon Rate converted from BitTypeCounter") {
    val bitTypeCounters: BitTypeCounters = BitTypeCounters(List(
      BitTypeCounter(5, 3),
      BitTypeCounter(6, 2),
      BitTypeCounter(3, 5),
      BitTypeCounter(2, 8),
      BitTypeCounter(2, 2)
    ))

    val expected = BinaryCoding(List(One, One, Zero, Zero, Zero))
    val epsilonRate = bitTypeCounters.epsilonRate


    assertEquals(epsilonRate, expected)
  }

  test("End to end test with test input case") {
    val bitTypeCounters = TEST_INPUT
      .map(BinaryCoding.apply)
      .sumBinaryCodingColumns

    val gammRate = bitTypeCounters.gammaRate.toInt
    val epsilonRate = bitTypeCounters.epsilonRate.toInt
    val powerConsumption = bitTypeCounters.powerConsumption

    val expectedGammarate = 22
    val expectedEpsilonRate = 9
    val expectedPowerRate = 198

    assertEquals(epsilonRate, expectedEpsilonRate)
    assertEquals(gammRate, expectedGammarate)
    assertEquals(powerConsumption, expectedPowerRate)
  }

  test("[TODO] Calculate power consumption") {
    assert(false)
  }
}
