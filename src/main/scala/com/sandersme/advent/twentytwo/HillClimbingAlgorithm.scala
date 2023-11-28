package com.sandersme.advent.twentytwo

import com.sandersme.advent.Input
import com.sandersme.advent.twentytwo.model.HillClimbing

object HillClimbingAlgorithm {

  def main(cmdLine: Array[String]): Unit = {
    val inputData = Input.readTwentyTwoFromResource("day12_input")

    val hillGrid = HillClimbing.parseInput(inputData)

    val shortestPath = HillClimbing.findShortestPath(hillGrid)

    println(s"Shortest Path: ${shortestPath}")

  }

}
