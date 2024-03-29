package com.sandersme.advent.twentytwo

import com.sandersme.advent.Input
import com.sandersme.advent.twentytwo.model.CrateSupply

object SupplyStacks {
  /**
   * The expedition can depart as soon as the final supplies have been unloaded from the ships. Supplies are stored in
   * stacks of marked crates, but because the needed supplies are buried under many other crates, the crates need
   * to be rearranged.
   * The ship has a giant cargo crane capable of moving crates between stacks. To ensure none of the crates get crushed
   * or fall over, the crane operator will rearrange them in a series of carefully-planned steps. After the crates are
   * rearranged, the desired crates will be at the top of each stack.
   * The Elves don't want to interrupt the crane operator during this delicate procedure, but they forgot to ask
   * her which crate will end up where, and they want to be ready to unload them as soon as possible so they can embark.
   * They do, however, have a drawing of the starting stacks of crates and the rearrangement procedure
   *
   * The Elves just need to know which crate will end up on top of each stack; in this example, the top crates are C in
   * stack 1, M in stack 2, and Z in stack 3, so you should combine these together and give the Elves the message CMZ.
   *
   * After the rearrangement procedure completes, what crate ends up on top of each stack?
   * To begin, get your puzzle input.
   */
  def main(args: Array[String]): Unit = {
    val input = Input.readTwentyTwoFromResource("day5_input")

    val crateSupply = CrateSupply.fromInputSeq(input)

    val topEachStackPartOne = crateSupply
      .moveForwardAllInstructionsPartOne
      .printTopEachStack

    println(s"After all the move instructions the top of each stack is: ${topEachStackPartOne}")

    val topEachStackPartTwo = crateSupply
      .moveForwardAllInstructionsPartTwo
      .printTopEachStack

    println(s"After all the move instructions the top of each stack is: ${topEachStackPartTwo}")

  }
}
