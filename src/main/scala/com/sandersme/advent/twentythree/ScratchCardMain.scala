package com.sandersme.advent.twentythree

import com.sandersme.advent.Input
import com.sandersme.advent.twentythree.model.ScratchCard

object ScratchCardMain { 
  def main(args: Array[String]): Unit = {
    val input = Input.readTwentyThreeFromResource("day4_input")

    val scratchCards = ScratchCard.parseInput(input)
    val totalScore = ScratchCard.calculateTotalScore(scratchCards)

    println(s"The total score from the scratch cards is: ${totalScore}")

    val totalSumOfCopies = ScratchCard.sumTotalCopies(scratchCards)

    println(s"The total sum of copies of scratch cards: ${totalSumOfCopies}")
  }

}
