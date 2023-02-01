package com.sandersme.advent.twentytwo

import com.sandersme.advent.twentytwo.model.CycleInstructions
import com.sandersme.advent.Input

object CathodeRayTube {

  /**
   * You avoid the ropes, plunge into the river, and swim to shore. The Elves yell something about meeting back up with
   * them upriver, but the river is too loud to tell exactly what they're saying. They finish crossing the bridge and disappear from view.
   * Situations like this must be why the Elves prioritized getting the communication system on your handheld device working.
   * You pull it out of your pack, but the amount of water slowly draining from a big crack in its screen tells you it probably won't be of much immediate use.
   * Unless, that is, you can design a replacement for the device's video system! It seems to be some kind of cathode-ray tube screen and simple CPU that are both driven by a precise clock circuit. The clock circuit ticks at a constant rate; each tick is called a cycle.
   * Start by figuring out the signal being sent by the CPU. The CPU has a single register, X, which starts with the value 1. It supports only two instructions:
   * addx V takes two cycles to complete. After two cycles, the X register is increased by the value V. (V can be negative.)
   * noop takes one cycle to complete. It has no other effect.
   * The CPU uses these instructions in a program (your puzzle input) to, somehow, tell the screen what to draw.
   * @param args
   */
  def main(args: Array[String]): Unit = {
    val input = Input.readTwentyTwoFromResource("day10_input")

    val signalStrengths = CycleInstructions
      .signalsStrengthFromInput(input, Set(20, 60, 100, 140, 180, 220))

    val sumOfSignalStrengths = signalStrengths
      .map{ case(key, value) =>
        key * value
      }.sum

    println(s"The signal strength of interesting signals is: ${sumOfSignalStrengths}")

    val instructions = CycleInstructions.parseInput(input)
    val processedInstructions = CycleInstructions.processInstructions(instructions)

    CycleInstructions.generatePixelLocations(processedInstructions)
  }
}
