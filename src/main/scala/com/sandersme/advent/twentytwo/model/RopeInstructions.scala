package com.sandersme.advent.twentytwo.model

import com.sandersme.advent.twentytwo.model.Direction.negativeDirection

case class RopeInstruction(direction: Direction, step: Int) {
  def stepMovement: Int = {
    if (negativeDirection(direction)) {
      -1
    } else {
      1
    }
  }
}

case class RopeInstructions(instructions: List[RopeInstruction])

object RopeInstructions {
  def empty: RopeInstructions = RopeInstructions(List.empty)

  def parseInstructions(input: List[String]): RopeInstructions = {
    val instructions = input
      .map{ line =>
        val split = line.split(" ")
        val direction = Direction.valueOf(split(0))
        val stepSize = split(1).toInt

        RopeInstruction(direction, stepSize)
      }

    RopeInstructions(instructions)
  }
}

