package com.sandersme.advent.twentythree

import com.sandersme.advent.Input
import com.sandersme.advent.twentythree.model.CamelPoker

object CamelCards {
  def main(args: Array[String]): Unit = {
    val start = System.currentTimeMillis()
    val input = Input.readTwentyThreeFromResource("day7_input")

    val pokerHands = CamelPoker.parse(input)
    val totalBets = CamelPoker.calculateTotalScore(pokerHands)
    val pokerHandsWithJokersRule = CamelPoker.parse(input, true)
    val totalBetsWithJokers = CamelPoker.calculateTotalScore(pokerHandsWithJokersRule)

    val stop = System.currentTimeMillis()
    println(s"The total amount from Camel Poker based on the pokerhand ranks: ${totalBets}")
    println(s"The total score from camel poker using Jokers is: ${totalBetsWithJokers}")
    println(s"Process took a total of ${stop - start}ms")

  }
}
