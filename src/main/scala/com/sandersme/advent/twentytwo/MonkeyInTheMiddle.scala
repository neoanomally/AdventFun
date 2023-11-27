package com.sandersme.advent.twentytwo

import com.sandersme.advent.Input
import com.sandersme.advent.twentytwo.model.Monkey

object MonkeyInTheMiddle {

  def main(args: Array[String]): Unit = {
    val input = Input.readTwentyTwoFromResource("day11_input")

    val parsedMonkies = Monkey.parseInput(input)

    val updatedMonkies = Monkey.iterateMonkeyInTheMiddleNTimes(parsedMonkies, 20, 3)
    val calculatedTopTwoMonkies = Monkey.multiplyBusiestMonkies(updatedMonkies)

    println(s"After twenty rounds of Monkey in the middle the top" +
      s"scores come out to: ${calculatedTopTwoMonkies}")

    val partTwoWorryFactor = Monkey.calculateLeastCommonMultiple(parsedMonkies)
    val partTwoUpdatedMonkies = Monkey.iterateMonkeyInTheMiddleNTimes(parsedMonkies,
      10000, partTwoWorryFactor)

    println(s"Worry Factor for part two is: ${partTwoWorryFactor}")
    val calculatedTopTwoMonkeiesPart2 = Monkey.multiplyBusiestMonkies(partTwoUpdatedMonkies)

    println(s"Part two after ten thousand rounds" +
      s"the monkey business factor is: ${calculatedTopTwoMonkeiesPart2}")
  }

}
