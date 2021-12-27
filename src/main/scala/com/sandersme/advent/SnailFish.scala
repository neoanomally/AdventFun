package com.sandersme.advent

object SnailFish {
  /**
   * * You descend into the ocean trench and encounter some snailfish. They say they
   * saw the sleigh keys! They'll even tell you which direction the keys went if
   * you help one of the smaller snailfish with his math homework.
   *
   * Snailfish numbers aren't like regular numbers. Instead, every snailfish
   * number is a pair - an ordered list of two elements. Each element of the pair
   * can be either a regular number or another pair.
   *
   * Pairs are written as [x,y], where x and y are the elements within the pair.
   * Here are some example snailfish numbers, one snailfish number per line:
   *
   * [1,2]
   * [[1,2],3]
* [9,[8,7]]
   * [[1,9],[8,5]]
   * [[[[1,2],[3,4]],[[5,6],[7,8]]],9]
   * [[[9,[3,8]],[[0,9],6]],[[[3,7],[4,9]],3]]
   * [[[[1,3],[5,3]],[[1,3],[8,7]]],[[[4,9],[6,9]],[[8,2],[7,3]]]]
   * This snailfish homework is about addition. To add two snailfish numbers, form
   * a pair from the left and right parameters of the addition operator. For
   * example, [1,2] + [[3,4],5] becomes [[1,2],[[3,4],5]].
   *
   * There's only one problem: snailfish numbers must always be reduced, and the process of adding two snailfish numbers can result in snailfish numbers that need to be reduced.
   *
   * To reduce a snailfish number, you must repeatedly do the first action in this list that applies to the snailfish number:
   *
   * If any pair is nested inside four pairs, the leftmost such pair explodes.
   * If any regular number is 10 or greater, the leftmost such regular number splits.
   * Once no action in the above list applies, the snailfish number is reduced.
   *
   * During reduction, at most one action applies, after which the process returns to the top of the list of actions. For example, if split produces a pair that meets the explode criteria, that pair explodes before other splits occur.
   *
   * To explode a pair, the pair's left value is added to the first regular number to
   * the left of the exploding pair (if any), and the pair's right value is added to
   * the first regular number to the right of the exploding pair (if any). Exploding
   * pairs will always consist of two regular numbers. Then, the entire exploding
   * pair is replaced with the regular number 0.
   *
   * To split a regular number, replace it with a pair; the left element of the
   * pair should be the regular number divided by two and rounded down, while the
   * right element of the pair should be the regular number divided by two and
   * rounded up. For example, 10 becomes [5,5], 11 becomes [5,6], 12 becomes [
   * 6,6], and so on.
   */

  def main(args: Array[String]): Unit = {

  }
}
