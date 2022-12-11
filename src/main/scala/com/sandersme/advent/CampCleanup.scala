package com.sandersme.advent

import com.sandersme.advent.twentytwo.model.ElvenCleanupPair

object CampCleanup {

  /**
   * Space needs to be cleared before the last supplies can be unloaded from the ships, and so several Elves have been
   * assigned the job of cleaning up sections of the camp. Every section has a unique ID number, and each Elf is assigned
   * a range of section IDs.
   * However, as some of the Elves compare their section assignments with each other, they've noticed that many of the
   * assignments overlap. To try to quickly find overlaps and reduce duplicated effort, the Elves pair up and make a
   * big list of the section assignments for each pair (your puzzle input).
   * Some of the pairs have noticed that one of their assignments fully contains the other. For example, 2-8 fully
   * contains 3-7, and 6-6 is fully contained by 4-6. In pairs where one assignment fully contains the other, one Elf
   * in the pair would be exclusively cleaning sections their partner will already be cleaning, so these seem like the
   * most in need of reconsideration. In this example, there are 2 such pairs.
   *
   * @param args
   */
  def main(args: Array[String]): Unit = {
    val input = Input.readTwentyTwoFromResource("day4_input")

    val assignments = ElvenCleanupPair.fromInput(input)
    val numFullyOverlappingPairs = assignments
      .map(_.containsFullyOverlappingCleanup)
      .count(_ == true)

    println(s"There are ${numFullyOverlappingPairs} assignments with fully overlapping Pairs.")

    val numPartiallyOverlappingPairs = assignments
      .map(_.containsPartiallyOverlappingCleanup)
      .count(_ == true)

    println(s"There are ${numPartiallyOverlappingPairs} assignments with partially overlapping Pairs.")

  }

}
