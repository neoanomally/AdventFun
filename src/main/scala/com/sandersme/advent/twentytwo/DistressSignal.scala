package com.sandersme.advent.twentytwo

import com.sandersme.advent.Input
import com.sandersme.advent.twentytwo.model.DistressSignalDecoder

object DistressSignal {
  def main(args: Array[String]): Unit = {
    val input = Input.readTwentyTwoFromResource("day13_input")

    val distressSignalDecoder = DistressSignalDecoder.parseInput(input)

    val sumOfCorrectPairs = distressSignalDecoder.sumOfCorrectOrderIndicies

    println(s"The sum of the correct pairs: ${sumOfCorrectPairs}")
  }
}
