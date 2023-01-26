package com.sandersme.advent.twentytwo

import com.sandersme.advent.Input
import com.sandersme.advent.twentytwo.model.RopeGrid

object RopeBridge {
  /**
   * This rope bridge creaks as you walk along it. You aren't sure how old it is, or whether it can even support your weight.
   * It seems to support the Elves just fine, though. The bridge spans a gorge which was carved out by the massive river far below you.
   * You step carefully; as you do, the ropes stretch and twist. You decide to distract yourself by modeling rope physics;
   * maybe you can even figure out where not to step.
   * Consider a rope with a knot at each end; these knots mark the head and the tail of the rope. If the head moves far
   * enough away from the tail, the tail is pulled toward the head.
   * Due to nebulous reasoning involving Planck lengths, you should be able to model the positions of the knots on a
   * two-dimensional grid. Then, by following a hypothetical series of motions (your puzzle input) for the head, you can determine how the tail will move.
   *
   * Due to the aforementioned Planck lengths, the rope must be quite short; in fact, the head (H) and tail (T) must
   * always be touching (diagonally adjacent and even overlapping both count as touching):
   *
   * @param args
   */
  def main(args: Array[String]): Unit = {
    /**
     * After simulating the rope, you can count up all of the positions the tail visited at least once.
     * In this diagram, s again marks the starting position (which the tail also visited) and # marks other positions
     * the tail visited
     */
    val input = Input.readTwentyTwoFromResource("day9_input")
    val ropeGrid = RopeGrid.parseInput(input)

    val incrementAllInstructions = ropeGrid.incrementAllInstructions

    val numberOfLocationsTailVisited = incrementAllInstructions.numPlacesTailVisited

    println(s"The number of places that the tail visited was: ${numberOfLocationsTailVisited}")

    /**
     * A rope snaps! Suddenly, the river is getting a lot closer than you remember. The bridge is still there, but some
     * of the ropes that broke are now whipping toward you as you fall through the air!
     * The ropes are moving too quickly to grab; you only have a few seconds to choose how to arch your body to avoid
     * being hit. Fortunately, your simulation can be extended to support longer ropes.
     * Rather than two knots, you now must simulate a rope consisting of ten knots. One knot is still the head of the
     * rope and moves according to the series of motions. Each knot further down the rope follows the knot in front of
     * it using the same rules as before.
     * Using the same series of motions as the above example, but with the knots marked H, 1, 2, ..., 9, the motions
     * now occur as follows:
     *
     * TODO: Instead of a head and one tail, we have a List and the tail is the end of the list. We keep track
     * of the coordinates of 10 knots.
     */

    val ropeGridPtTwo = RopeGrid.parseInput(input, 9)

    val partTwoIncrementAllInstructions = ropeGridPtTwo.incrementAllInstructions
    val numberOfLocationsTailVisitedPartTwo = partTwoIncrementAllInstructions.numPlacesTailVisited

    println(s"The number of places that the tail visited with 9 knows was: ${numberOfLocationsTailVisitedPartTwo}")
  }

}
