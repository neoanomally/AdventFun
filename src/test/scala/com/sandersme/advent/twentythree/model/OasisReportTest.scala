package com.sandersme.advent.twentythree.model

import com.sandersme.advent.twentythree.model.OasisReport.parseInput

class OasisReportTest extends munit.FunSuite {
  test("Validate that we can parse the test input into an OasisReport") {
    val parsedData = OasisReport.parseInput(TEST_INPUT)

    val expectedValues = Vector(
      Vector(0, 3, 6, 9, 12, 15),
      Vector(1, 3, 6, 10, 15, 21),
      Vector(10, 13, 16, 21, 30, 45)
    ) 
  }

  test("Calculate the sum of the numbers in a sequence should equal 114") {
    val expectedResult = 114
    val parsedData = OasisReport.parseInput(TEST_INPUT)
    val sumOfNext = parsedData.calculateSumOfNext

    assertEquals(sumOfNext, expectedResult)
  }

  test("Validate that we can calculate the previous number in each sequence") {
    val parsedData = OasisReport.parseInput(TEST_INPUT)
    val previousNumber = parsedData.history
      .map(OasisReport.calculatPreviousNumberInSequence)
    
    val expectedPreviousNumbers = Vector(-3, 0, 5)

    assertEquals(previousNumber, expectedPreviousNumbers)
  } 

  test("Validate that the sum of previous numbers should equal 2") {
    val parsedData = OasisReport.parseInput(TEST_INPUT)
    val sumOfPreviousNumbers = parsedData.calculateSumOfPrevious

    val expectedSumOfPrevious = 2

    assertEquals(sumOfPreviousNumbers, expectedSumOfPrevious)
  }


  test("Validate that we can calculate the next number in each sequence") {
    val parsedData = OasisReport.parseInput(input = TEST_INPUT)
    val lastNumbers = parsedData.history
      .map(OasisReport.calculateNextNumberInSequence)

    val expectedLastNumbers = Vector(18, 28, 68)
    assertEquals(lastNumbers, expectedLastNumbers)
  }

  test("Validate that we have the forrect layered sequence when finding the difference in a sequence") {
    val parsedData = OasisReport.parseInput(TEST_INPUT)
    val idxOne = parsedData.history(1)

    val layers = OasisReport.calculateLayersForLine(idxOne)

    val expectedLayers = Vector(
      Vector(1, 3, 6, 10, 15, 21),
      Vector(2, 3, 4, 5, 6),
      Vector(1, 1, 1, 1),
      Vector(0, 0, 0)
    )

    assertEquals(layers, expectedLayers)
  }

  val TEST_INPUT = """0 3 6 9 12 15
    |1 3 6 10 15 21
    |10 13 16 21 30 45""".stripMargin
      .split("\n")
      .toList
}
