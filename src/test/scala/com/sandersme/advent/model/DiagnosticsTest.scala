package com.sandersme.advent.model

import com.sandersme.advent.model.BinaryCoding.{One, Zero}

class DiagnosticsTest extends munit.FunSuite {
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


  test("Gamma Rate converted from BitTypeCounter") {
    val diagnostics = Diagnostics.fromBinaryInput(TEST_INPUT)


    val expected = BinaryCoding(List(One, Zero, One, One, Zero))
    val gammaRate = diagnostics.gammaRate

    assertEquals(gammaRate, expected)
  }

  test("Episolon Rate converted from BitTypeCounter") {
    val diagnostics = Diagnostics.fromBinaryInput(TEST_INPUT)

    val expected = BinaryCoding(List(Zero, One, Zero, Zero, One))
    val epsilonRate = diagnostics.epsilonRate

    assertEquals(epsilonRate, expected)
  }

  /**
   * TODO MOVE Diagnostics Specific tests to it's own test file
   */
  test("End to end test with test input case for power consumption") {
    val diagnostics = Diagnostics.fromBinaryInput(TEST_INPUT)

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
