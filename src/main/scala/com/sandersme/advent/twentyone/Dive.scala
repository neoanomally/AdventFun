package com.sandersme.advent.twentyone

import com.sandersme.advent.twentyone.Input.{parseDay2Input, readFromDataResource, readFromResource}
import com.sandersme.advent.twentyone.model.{Direction, PilotCommand, Position}

/**
 * Now, you need to figure out how to pilot this thing.
 *
 * It seems like the submarine can take a series of commands like forward 1, down 2, or up 3:
 *
 * forward X increases the horizontal position by X units.
 *  down X increases the depth by X units.
 * up X decreases the depth by X units.
 * Note that since you're on a submarine, down and up affect your depth, and so they have the opposite
 * result of what you might expect.
 *
 * The submarine seems to already have a planned course (your puzzle input). You should probably figure out where it's
 * going. For example:
 *
 * forward 5
 * down 5
 * forward 8
 * up 3
 * down 8
 * forward 2
 * Your horizontal position and depth both start at 0. The steps above would then modify them as follows:
 *
 * forward 5 adds 5 to your horizontal position, a total of 5.
 * down 5 adds 5 to your depth, resulting in a value of 5.
 * forward 8 adds 8 to your horizontal position, a total of 13.
 * up 3 decreases your depth by 3, resulting in a value of 2.
 * down 8 adds 8 to your depth, resulting in a value of 10.
 * forward 2 adds 2 to your horizontal position, a total of 15.
 * After following these instructions, you would have a horizontal position of 15 and a depth of 10.
 * (Multiplying these together produces 150.)
 *
 *  Calculate the horizontal position and depth you would have after following the planned course.
 *  What do you get if you multiply your final horizontal position by your final depth?
 */

object Dive {
  val DAY_2_INPUT = "day2_input"

  def main(args: Array[String]): Unit = {
    val dayTwoInput: List[String] = readFromDataResource(DAY_2_INPUT)

    val parsedInput = dayTwoInput.map(Input.parseDay2Input)
    val partTwoPosition = Position.calculatePosition(parsedInput)

    val position = calculateFinalPosition(dayTwoInput)

    println(s"Final position: $position with a multiple of: ${position.multiplyPosition}")
    println(s"Final position for part two: $partTwoPosition with multiple" +
      s"of ${partTwoPosition.multiplyPosition}")
  }

  /**
   * Below are the first portions of the day 2 dive. I did not update these during the refactor
   * since the part two is the final piece. These were left here for posterity.
   */
  def calculateFinalPosition(input: List[String]): Position = {
    val pilotCommands = input.map(parseDay2Input)
    val sumPilotCommands = sumPilotCommand(pilotCommands)
    calculatePosition(sumPilotCommands)
  }

  def sumPilotCommand(pilotCommands: List[PilotCommand]): Map[Direction, Int] = {
    pilotCommands
      .groupBy(_.direction)
      .map{ case (direction, values) =>
        direction -> values.map(_.units).sum
      }
  }

  def calculatePosition(pilotCommands: Map[Direction, Int]): Position = {
    import Direction._

    val positions: List[Position] = pilotCommands.map{ case (direction, sum) =>
      direction match {
        case Forward => Position(sum, 0, 0)
        case Down => Position(0, sum, 0)
        case Up => Position(0, -sum, 0)
        case Left => assert(false, "Error the submarine should not go LEFT / Backwards.")
      }
    }.toList

    sumPositions(positions)
  }

  def sumPositions(positions: List[Position]): Position = {
    positions.foldLeft(Position.EMPTY_POSITION) {
      (left, right) =>
        val depth = left.depth + right.depth
        val horizontal = left.horizontal + right.horizontal
        val aim = left.aim + right.aim
        Position(horizontal, depth, aim)
    }
  }
}
