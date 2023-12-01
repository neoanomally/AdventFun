package com.sandersme.advent.twentythree.model

import com.sandersme.advent.twentythree.model.CalibrationInstruction.IndexValue
import junit.framework.TestCase.assertTrue

class TrebuchetCalibrationTest extends munit.FunSuite {

  val TEST_INPUT = """1abc2
                     |pqr3stu8vwx
                     |a1b2c3d4e5f
                     |treb7uchet""".stripMargin
    .split("\n")
    .toList

  val TEST_INPUT_WITH_LETTERS: List[String] = """two1nine
                                  |eightwothree
                                  |abcone2threexyz
                                  |xtwone3four
                                  |4nineeightseven2
                                  |zoneight234
                                  |7pqrstsixteen"""
    .stripMargin
    .split("\n")
    .toList

  test("Validate that we get the correct answer for 1abc2") {
    val instruction = CalibrationInstruction("1abc2")

    val calibrationValues = instruction.findFrontAndBack
    val expectedValues = CalibrationValues(1, 2)

    assertEquals(calibrationValues, expectedValues)
  }

  test ("validate we get the answer 7, 7 for treb7uchet") {
    val instruction = CalibrationInstruction("treb7uchet")

    val results = instruction.findFrontAndBack
    val expectedValues = CalibrationValues(7, 7)

    assertEquals(results, expectedValues)
  }


  test("validate we get the answer 77 for treb7uchet when calling number") {
    val instruction = CalibrationInstruction("treb7uchet")

    val results = instruction.findFrontAndBack.number
    val expectedValues = 77

    assertEquals(results, expectedValues)
  }

  test("Validate that we parse Instructions") {

    TrebuchetCalibration.parseInput(TEST_INPUT)
    val calibrations: List[CalibrationValues] = TrebuchetCalibration.parseInput(TEST_INPUT)

    val expectedLast = CalibrationValues(7, 7)
    assertEquals(calibrations.last, expectedLast)
  }

  test("Validate that we can sum all calibration values") {
    val values = TrebuchetCalibration.parseInput(TEST_INPUT)

    val sum = TrebuchetCalibration.sumCalibrationNumbers(values)
    val expectedSum = 142

    assertEquals(sum, expectedSum)
  }

  test("Validate that we can translate the letters") {
    assertEquals(TEST_INPUT_WITH_LETTERS.size, 7)
    val values = TrebuchetCalibration.parseInputTranslated(TEST_INPUT_WITH_LETTERS.take(1))

    val head = values.head
    val expectedHead = CalibrationValues(2, 9)

    assertEquals(head, expectedHead)
  }

  test ("Validate translated letters sum to 281") {
    val values = TrebuchetCalibration.parseInputTranslated(TEST_INPUT_WITH_LETTERS)
    val expectedSum = 281

    val sum = TrebuchetCalibration.sumCalibrationNumbers(values)

    assertEquals(sum, expectedSum)
  }

  test("Validate Translated letters two1nine into number and index") {
    val input = "two1nine"

    val translated = CalibrationInstruction.findTextIndicies(input, "nine")
    val expectedResults = ("nine", List(4))

    assertEquals(translated, expectedResults)
  }

  test("Validate that we can get all the letters from a eightwothree into numbers") {
    val input = "eightwothree"

    val eight = CalibrationInstruction.findTextIndicies(input, "eight")._2.head
    val two = CalibrationInstruction.findTextIndicies(input, "two")._2.head
    val three = CalibrationInstruction.findTextIndicies(input, "three")._2.isEmpty

    val expectedEight = 0
    val expectedTwo = 4

    val twoTwoInput = "twotwo"
    val twoTwoTwoInput = "twotwotwo"

    val twoTwo = CalibrationInstruction.findTextIndicies(twoTwoInput, "two")._2
    val twoTwoTwo = CalibrationInstruction.findTextIndicies(twoTwoTwoInput, "two")._2
    val expectedTwoTwoTwo = List(0, 3, 6)
    val expectedTwoTwo = List(0, 3)

    assertEquals(three, false)
    assertEquals(eight, expectedEight)
    assertEquals(two,expectedTwo)
    assertEquals(twoTwo, expectedTwoTwo)
    assertEquals(twoTwoTwo, expectedTwoTwoTwo)
  }

  test("Find All Number Indicies a1b2c3d4e5f") {
    val input = "a1b2c3d4e5f"
    val indicies = CalibrationInstruction.findAllDigitIndicies("a1b2c3d4e5f")

    val expectedIndicies = List(IndexValue(1, 1), IndexValue(3, 2), IndexValue(5, 3),
      IndexValue(7, 4), IndexValue(9, 5))



    assertEquals(indicies, expectedIndicies)
  }
}
