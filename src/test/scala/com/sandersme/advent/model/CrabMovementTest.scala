package com.sandersme.advent.model

class CrabMovementTest extends munit.FunSuite {
  val TEST_INPUT = List(16,1,2,0,4,2,7,1,2,14)

  test("Validate the default input maps correctly") {
    val input = "16,1,2,0,4,2,7,1,2,14"
    val parsedInput = CrabMovement.parseInput(input)

    assertEquals(parsedInput.crabs, TEST_INPUT)
  }

  test("Find the shortest distance of fuel using.") {
    val expectedMovementCost = CrabMovement.MovementCost(2, 37)
    val movementCost = CrabMovement(TEST_INPUT)
      .findMinimalFuelPosition

    assertEquals(movementCost, expectedMovementCost)
  }

}
