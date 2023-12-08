package com.sandersme.advent.twentythree

import com.sandersme.advent.Input
import com.sandersme.advent.twentythree.model.AlmanacMap
import com.sandersme.advent.twentythree.model.Almanac

object IfYouGiveASeedAFertalizer { 
  def main(args: Array[String]): Unit = {
    println("Starting If you give a Seed a Fertalizer")
    val input = Input.readTwentyThreeFromResource("day5_input")

    val almanacMap = Almanac.parseInput(input)

    val lowestLocation = almanacMap.findLowestLocation

    println(s"The lowest location from the input data is: ${lowestLocation}")
  }
}
