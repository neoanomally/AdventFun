package com.sandersme.advent.twentytwo

import com.sandersme.advent.Input
import com.sandersme.advent.twentytwo.model.Rucksack
/**
 * One Elf has the important job of loading all of the rucksacks with supplies for the jungle journey. Unfortunately,
 * that Elf didn't quite follow the packing instructions, and so a few items now need to be rearranged.
 * Each rucksack has two large compartments. All items of a given type are meant to go into exactly one of the two
 * compartments. The Elf that did the packing failed to follow this rule for exactly one item type per rucksack.
 * The Elves have made a list of all of the items currently in each rucksack (your puzzle input), but they need your
 * help finding the errors. Every item type is identified by a single lowercase or uppercase letter (that is, a and
 * A refer to different types of items).
 * The list of items for each rucksack is given as characters all on a single line. A given rucksack always has the
 * same number of items in each of its two compartments, so the first half of the characters represent items in the
 * first compartment, while the second half of the characters represent items in the second compartment.
 *
 * --- Part Two ---
 * As you finish identifying the misplaced items, the Elves come to you with another issue.
 * For safety, the Elves are divided into groups of three. Every Elf carries a badge that identifies their group.
 * For efficiency, within each group of three Elves, the badge is the only item type carried by all three Elves.
 * That is, if a group's badge is item type B, then all three Elves will have item type B somewhere in their rucksack,
 * and at most two of the Elves will be carrying any other item type.

 * The problem is that someone forgot to put this year's updated authenticity sticker on the badges. All of the badges
 * need to be pulled out of the rucksacks so the new authenticity stickers can be attached.
 * Additionally, nobody wrote down which item type corresponds to each group's badges. The only way to tell which item type
 * is the right one is by finding the one item type that is common between all three Elves in each group.


 */
object RucksackReorganization {

  def main(args: Array[String]): Unit = {

    /** Part 1: Find the item type that appears in both compartments of each rucksack. What is the sum of
     * the priorities of those item types? */
    val lines = Input.readTwentyTwoFromResource("day3_input")
    val sum = Rucksack.sumFromInput(lines)

    println(s"The sum of allr rucksacks is: ${sum}")

    import Rucksack._

    val commonItemFromBadges = Rucksack.createTripletsFromInput(lines)
      .sumCommonItems

    println(s"The sum of all triplets ${commonItemFromBadges}")
  }

}
