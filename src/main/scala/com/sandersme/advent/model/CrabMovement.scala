package com.sandersme.advent.model

import com.sandersme.advent.model.CrabMovement.MovementCost

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
   * @return the optimal MovementCost
   */
  def findMinimalFuelPosition: MovementCost = {
    val start = crabs.min
    val end = crabs.max
    val DEFAULT_MOVEMENT_COST = MovementCost()

    (start to end)
      .foldLeft(DEFAULT_MOVEMENT_COST){ case (movementCost, nextPosition) =>

        val cost = crabs.map(position => Math.abs(nextPosition - position)).sum
        if (cost < movementCost.cost) {
          MovementCost(nextPosition, cost)
        } else {
          movementCost
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
}