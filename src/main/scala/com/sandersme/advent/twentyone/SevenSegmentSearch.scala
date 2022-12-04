package com.sandersme.advent.twentyone

import com.sandersme.advent.Input
import com.sandersme.advent.twentyone.model.SevenSegment

object SevenSegmentSearch {
  def main(args: Array[String]): Unit = {
    val input = Input.readTwentyOneFromResource("day8_input")

    val parsedSevenSegment = SevenSegment.parseInput(input)

    val mappingResults  = parsedSevenSegment.flatMap{ sevenSegment =>
      val mappingResult = SevenSegment.calculateMappingResult(sevenSegment)

      mappingResult.map( mapped =>
        mapped.outputNumbers
        .foldLeft("")((res, v) => res + v)
      )
    }

    println(mappingResults)

    println(s"\nThe sum of all the results are ${mappingResults.map(_.toInt).sum.toString}")
  }

}
