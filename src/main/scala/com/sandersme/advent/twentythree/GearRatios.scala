package com.sandersme.advent.twentythree

import com.sandersme.advent.Input
import com.sandersme.advent.twentythree.model.EngineSchematic

object GearRatios {
  def main(args: Array[String]): Unit = {
    val input = Input.readTwentyThreeFromResource("day3_input")

    val engineSchematic = EngineSchematic.parseInput(input)

    val sumOfValidNumbers = engineSchematic.sumOfValidParts

    println(s"The sum of valid parts numbers: ${sumOfValidNumbers}")

    val sumOfGearRatios = engineSchematic.sumOfGearRatios
    println(s"The sum of Gear Ratios is ${sumOfGearRatios}")
  }
}
