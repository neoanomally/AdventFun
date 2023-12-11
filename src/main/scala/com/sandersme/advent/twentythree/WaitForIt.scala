package com.sandersme.advent.twentythree

import com.sandersme.advent.Input
import com.sandersme.advent.twentythree.model.BoatRace

object WaitForIt { 
  def main(args: Array[String]): Unit = {
    val input = Input.readTwentyThreeFromResource("day6_input")

    val boatRace = BoatRace.parseBoatRace(input)

    val mulitplyNumWaysToWin = boatRace.multiplyWaysToWin

    println(s"The number of ways to win the race product: ${mulitplyNumWaysToWin}")

    val singleBoatRaceStats = BoatRace
      .parseSingleBoatRace(input)
      .raceStats
      .head

    val numberOfWaysToWin = singleBoatRaceStats.calculateTotalWaysToWin
    println("Woops read the original input incorrectly. As a Single" + 
      s"Race. The total number of ways to win is equal to: ${numberOfWaysToWin}")
  }
}
