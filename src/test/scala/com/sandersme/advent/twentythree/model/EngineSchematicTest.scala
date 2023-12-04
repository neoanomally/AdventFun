package com.sandersme.advent.twentythree.model

import com.sandersme.advent.twentythree.model.{PNum, Period, PSymbol}
import munit.FunSuite

class EngineSchematicTest extends FunSuite {
  val TEST_INPUT: List[String] = """467..114..
                     |...*......
                     |..35..633.
                     |......#...
                     |617*......
                     |.....+.58.
                     |..592.....
                     |......755.
                     |...$.*....
                     |.664.598..""".stripMargin
    .trim
    .split("\n")
    .toList

  test("Validate that we can parse the test input into correct engine schmeatic") {
    val engineSchematic = EngineSchematic.parseInput(TEST_INPUT)


    val zeroZero = engineSchematic.getPart(2, 0) // 7
    val expectedPartPoint = PartPoint(2, 0)
    val expectedValue = PNum(7)

    assertEquals(zeroZero.partPoint, expectedPartPoint)
    assertEquals(zeroZero.value, expectedValue)
  }

  test("Another find all neighbors test for a few edge cases") {
    val partA = PartPoint(0, 0)
    val partB = PartPoint(1, 8)
    val partC = PartPoint(5, 5)

    val maxX = 9
    val maxY = 9

    val neighborsA = EngineSchematic.findAllNeighbors(partA.x, partA.y, maxX, maxY)
    val neighborsB = EngineSchematic.findAllNeighbors(partB.x, partB.y, maxX, maxY)
    val neighborsC = EngineSchematic.findAllNeighbors(partC.x, partC.y, maxX, maxY)

    val expectedNeighborsA = List(
      PartPoint(0, 1),  PartPoint(1, 0), PartPoint(1, 1)
    )
    val expectedNeighborsB = List(
      PartPoint(0, 7), PartPoint(0, 8), PartPoint(1, 7), PartPoint(2, 7),  PartPoint(2, 8)
    )
    val expectedNeighborsC = List(
      PartPoint(4, 4), PartPoint(4, 5), PartPoint(4, 6), PartPoint(5, 4), PartPoint(5, 6),
      PartPoint(6, 4), PartPoint(6, 5),PartPoint(6, 6)
    )

    assertEquals(neighborsA, expectedNeighborsA)
    assertEquals(neighborsB, expectedNeighborsB)
    assertEquals(neighborsC, expectedNeighborsC)
  }

  test("Validate we can find all contiguous numbers in each line") {
    // TODO would be good to test Processing State.
  }

  test("Find all numbers all the indicies for each contiguous number") {
    val engineSchematic = EngineSchematic.parseInput(TEST_INPUT)

    val allNumbers = engineSchematic
      .findAllNumbers
      .map(_.value)

    val expectedNumbers = List(467, 114, 35, 633, 617, 58, 592, 755, 664, 598)

    assertEquals(allNumbers, expectedNumbers)
  }

  test("Find all Valid numbers; which is any number that has neighbors that are symbols") {
    val engineSchematic = EngineSchematic.parseInput(TEST_INPUT)

    val sumOfParts = engineSchematic
      .sumOfValidParts

    val expectedSumOfParts = 4361

    assertEquals(sumOfParts, expectedSumOfParts)
  }

  test("The sum of all gear ratios for test should equal 467835") {
    val engineSchematic = EngineSchematic.parseInput(TEST_INPUT)

    val sumOfGearRatios = engineSchematic.sumOfGearRatios
    val expectedSum = 467835

    assertEquals(sumOfGearRatios, expectedSum)
  }
}
