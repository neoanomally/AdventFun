package com.sandersme.advent.twentyone.model

import com.sandersme.advent.twentyone.model.CrabMovement.MovementCost

/**
 * This is going to contain the logic for how we move crabs across a horizontal plane.
 * For the second solution I'll try something more complex, but for the beginning we
 * can brute force loop through each of the distinct positions.
 *
 * For a more optimized solution we can search for tightly packed clusters of crabs.
 * For example finding the Median location of said crabs.
 */
case class CrabMovement(crabs: List[Int]) {
  /**
   * Start with max value movement and fold through each position until  we find the optimal
   * horizontal position.
   *
   * TODO early escaping of the total cost already outweighs the smallest bit.
   * @return the optimal MovementCost
   */
  def findMinimalFuelPosition: MovementCost = {
    val start = crabs.min
    val end = crabs.max
    val DEFAULT_MOVEMENT_COST = MovementCost()

    val stepsFuelCostMap: Map[Int, Int] = generateFuelCostMap(start, end)

    (start to end)
      .foldLeft(DEFAULT_MOVEMENT_COST){ case (movementCost, nextPosition) =>

        val totalCost = crabs.map { position =>
          val steps = Math.abs(nextPosition - position)
          stepsFuelCostMap(steps)
        }.sum

        if (totalCost < movementCost.cost) {
          MovementCost(nextPosition, totalCost)
        } else {
          movementCost
        }
      }
  }

  private[model] def generateFuelCostMap(start: Int, end: Int): Map[Int, Int] = {
    (start to end)
      .foldLeft(Map(0 -> 0)){ case(fuelCostMap, nextStep) =>
        if (fuelCostMap.contains(nextStep - 1)) {
          val fuelCost = fuelCostMap(nextStep - 1) + nextStep
          fuelCostMap + (nextStep -> fuelCost)
        } else {
          fuelCostMap + (nextStep -> CrabMovement.calculateFuelCost(0, nextStep))
        }
      }
  }


}

object CrabMovement {
  case class MovementCost(position: Int = -1, cost: Int = Integer.MAX_VALUE)

  def parseInput(input: String): CrabMovement = {
    CrabMovement(
      input.split(",")
        .map(_.toInt)
        .toList
    )
  }

  def calculateFuelCost(start: Int, stop: Int): Int = {
    val numberOfSteps = Math.abs(start - stop)

    (1 to numberOfSteps)
      .zipWithIndex
      .map{ case(_, idx) => idx + 1}
      .sum
  }
}