package com.sandersme.advent.twentythree 

import com.sandersme.advent.Input
import com.sandersme.advent.twentythree.model.DesertNetwork

object HauntedWasteland {
  def main(args: Array[String]): Unit = {
    val input = Input.readTwentyThreeFromResource("day8_input")

    val desertNetwork = DesertNetwork.parseInput(input)

    val numStepsToExit = desertNetwork.findStepsToExit
    println(s"The number of steps to exit the desert is: ${numStepsToExit}")

    // val numGhostStepsToExit = desertNetwork.findGhostStepsToExit

    // println(s"The number of ghosts steps to exit the desert is: ${numGhostStepsToExit}")
    // The following are the prime factorization for the set of numbers
    // 1, 79, 293, 23147
    // 1, 67, 293, 19631
    // 43, 293
    // 73, 293
    // 61, 293
    // 71, 293
    desertNetwork.findCyclesAndExitPoints.foreach(println)
    val cycles = desertNetwork.calculateStepsFromCycles

    println(s"The number of steps from cycles is: ${cycles}")
    
    
  }
}
