package com.sandersme.advent.twentyone


import com.sandersme.advent.Input
import com.sandersme.advent.twentyone.Dive.calculatePosition
import com.sandersme.advent.twentyone.model.{Direction, Position}

class DiveTest extends munit.FunSuite {
  val DAY_TWO_TEST_INPUT = List(
    "forward 5",
    "down 5",
    "forward 8",
    "up 3",
    "down 8",
    "forward 2"
  )

  test("Parse input without issues") {
    val parsedInput = DAY_TWO_TEST_INPUT.map(Input.parseDay2Input)

    assertEquals(parsedInput.size, 6)
  }

  test("Test end to end position") {
    val expectedFinalPosition = Position(15, 10, 0)

    val position = Dive.calculateFinalPosition(DAY_TWO_TEST_INPUT)

    assertEquals(position, expectedFinalPosition)
  }

  test("Save multiple positions") {
    val testPositions = List(
      Position(10, 5, 0),
      Position(5, -20, 0),
      Position(0, 0, 0)
    )

    val expectedPosition = Position(15, -15, 0)
    val summedPosition = Dive.sumPositions(testPositions)

    assertEquals(expectedPosition, summedPosition)
  }

  test("Calculate Map of positions to positions") {
    val testInput: Map[Direction, Int] = Map(
      Direction.Forward -> 100,
      Direction.Up -> 150,
      Direction.Down -> 500
    )

    val expectedPositions = Position(100, 350, 0)
    val calculatedPositions: Position = calculatePosition(testInput)

    assertEquals(expectedPositions, calculatedPositions)
  }

  test("Calculate position using aim") {
    val parsedInput = DAY_TWO_TEST_INPUT.map(Input.parseDay2Input)

    val finalPosition = Position.calculatePosition(parsedInput)
    val expectedPosition = Position(15, 60, 10)
    val expectedMultiply = 900

    assertEquals(finalPosition, expectedPosition)
    assertEquals(finalPosition.multiplyPosition, expectedMultiply)
  }
}
