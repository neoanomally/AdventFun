package com.sandersme.advent.model

class CrabMovementTest extends munit.FunSuite {
  val TEST_INPUT = List(16,1,2,0,4,2,7,1,2,14)

  test("Validate the default input maps correctly") {
    val input = "16,1,2,0,4,2,7,1,2,14"
    val parsedInput = CrabMovement.parseInput(input)

    assertEquals(parsedInput.crabs, TEST_INPUT)
  }

  test("Find the shortest distance of fuel using.") {
    val expectedMovementCost = CrabMovement.MovementCost(5, 168)
    val movementCost = CrabMovement(TEST_INPUT)
      .findMinimalFuelPosition

    assertEquals(movementCost, expectedMovementCost)
  }

  test("calculate the steps for 3 should cost 6 fuel") {
    val expectedCost = 6
    val fuelCost = CrabMovement.calculateFuelCost(2, 5)

    val expectedSecondCost = 66
    val fuelCostElevenSteps = CrabMovement.calculateFuelCost(16, 5)

    assertEquals(fuelCost, expectedCost)
    assertEquals(fuelCostElevenSteps, expectedSecondCost)
  }

  test("Generate full hashmap for quick fuel cost lookups") {
    val costMap = CrabMovement(TEST_INPUT)
      .generateFuelCostMap(2, 20)

    val valueForThreeSteps = costMap(3)
    val expectedThreeValue = 6

    val valueForElevenSteps = costMap(11)
    val expectedValueForEleven = 66

    assertEquals(valueForThreeSteps, expectedThreeValue )
    assertEquals(valueForElevenSteps, expectedValueForEleven)
  }

}
