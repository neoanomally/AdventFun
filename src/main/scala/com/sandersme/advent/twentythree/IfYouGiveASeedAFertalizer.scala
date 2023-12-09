package com.sandersme.advent.twentythree

import com.sandersme.advent.Input
import com.sandersme.advent.twentythree.model.AlmanacMap
import com.sandersme.advent.twentythree.model.Almanac

object IfYouGiveASeedAFertalizer { 
  def main(args: Array[String]): Unit = {
    val startTime = System.currentTimeMillis()
    println("Starting If you give a Seed a Fertalizer")
    val input = Input.readTwentyThreeFromResource("day5_input")

    val almanac = Almanac.parseInput(input)

    val lowestLocation = almanac.findLowestLocation
    val lowestRangeLocation = almanac.findSeedRangeMinLocation
    val endTime = System.currentTimeMillis()
    val runtime = endTime - startTime
    println(s"The lowest location from the input data is: ${lowestLocation}")
    println(s"The total number of seeds: ${almanac.countTotalNumberSeeds}")

    println(s"The lowest location from SeedRanges in input data is: ${lowestRangeLocation}")
    println(s"Solution took ${runtime}ms to run") 
  }
}
