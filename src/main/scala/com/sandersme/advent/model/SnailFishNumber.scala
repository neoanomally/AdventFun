package com.sandersme.advent.model

import com.sandersme.advent.model.SnailFishNumber.{bubbleUpValue, recursiveReduce, snailFishNumberAsInt, split, traverseExplodeSplit, tupleToPropogate}

import scala.annotation.tailrec

type SnailFishNumbers = List[SnailFishNumber]

/**
 * Perfect opportunity to do union pairs. Left and right can either be
 * a value or a pair of values. Order matters for snailfishNumbers.
 *
 * @param left left side of a SnailFish number
 * @param right right side of a SnailFish number.
 */
case class SnailFishNumber(left: Int | SnailFishNumber, right:  Int | SnailFishNumber) {
  override def toString: String = s"($left, $right)"


  // TODO We don't actually handle the edge case where the root left and right should be split
  private[model] def explodeAndSplitRoot: SnailFishNumber = {
    val shouldReduceLeft = SnailFishNumber
      .findRootBranchDepth(this)._1 >= 5
    val shouldReduceRight = !shouldReduceLeft && SnailFishNumber
      .findRootBranchDepth(this)._2 >= 5

    val (leftReduced, leftBubbled) = SnailFishNumber.traverseExplodeSplit(left, shouldReduceLeft)
    val (rightReduced, rightBubbled) = SnailFishNumber.traverseExplodeSplit(right, shouldReduceRight)

    /// TODO I think we need to handle the case where left left branch is Int
    /// ANd the case where right right branch is Int
    val finalLeft = SnailFishNumber.mergeExploded(leftReduced, rightBubbled._1)
    val finalRight = SnailFishNumber.mergeExploded(rightReduced, leftBubbled._2)

    SnailFishNumber(finalLeft, finalRight)
  }

  def reduce: SnailFishNumber = {
    recursiveReduce(this)
  }

  /**
   * The following three methods are really just helper methods to determine wheter to keep reducing
   * We could do this as part of our reduce method, but the problem is it's a bit harder to reason
   * about everything. Because we don't expecct extremely large nested values It's okay
   * for these to both happen at the same time. willReduce && willExplode could technically
   * happen at the same time. This was just easier. The other thing that could happen
   * is that we can send the predicate with each method. Another reason I chose this way is because
   * I wanted to write some quick methods for testing maximum depth as well s max size.
   * @return
   */
  private[model] def willReduce: Boolean = {
    SnailFishNumber.willExplode(this) || SnailFishNumber.willSplit(this)
  }
}

object SnailFishNumber {

  /**
   * This will continue to reduce until we have reduced as much as possible.
   *
   * @param snailFishNumbers
   * @return
   */
  @tailrec
  def recursiveReduce(snailFishNumber: SnailFishNumber): SnailFishNumber = {
    if (snailFishNumber.willReduce) {
      println(s"Reducing: $snailFishNumber")
      recursiveReduce(snailFishNumber.explodeAndSplitRoot)
    } else {

      println(s"Final: $snailFishNumber")
      snailFishNumber
    }
  }

  def parseInput(input: List[String]): SnailFishNumber = {
    val allPairs = input.map(parsePairs).map(_.reduce)

    val finalResult = allPairs.tail
      .foldLeft(allPairs.head){case (snailFishAccum, snailFish) =>
        println(s"Addition: ${SnailFishNumber(snailFishAccum, snailFish)}")
        SnailFishNumber(snailFishAccum, snailFish).reduce
      }

    finalResult
  }


  /** Any branch from the root tree that is 4 deep should explode. */
  private[model] def willExplode(snailFishNumber: SnailFishNumber): Boolean = {
    val depth = SnailFishNumber.findDepth(snailFishNumber)

    depth >= 5
  }

  /** Check if the maximum number in any portion of the tree is higher than 10 then explode it */
  private[model] def willSplit(snailFishNumber: SnailFishNumber): Boolean = {
    SnailFishNumber.maxNumberInTree(snailFishNumber) >= 10
  }

  /**
   * This method is to find the depth of a particular branch. This is probably just
   * a bandaid, may need a way to figure out which branch explodes each iteration
   * before traversing then only exploding that branch :|  So dumb
   *
   * @param snailFishNumber input snailfishNumber
   * @param goLeft Boolean value to indicate if we go left or right
   * @param depth of branch side.
   */
  private[model] def findRootBranchDepth(snailFishNumber: SnailFishNumber | Int): (Int, Int) = {
    snailFishNumber match {
      case sfn: SnailFishNumber =>
        val leftDepth = findDepth(sfn.left, 1)
        val rightDepth = findDepth(sfn.right, 1)

        (leftDepth, rightDepth)
      case _ => throw new Exception("findBranchDepth should only happen on the root branch")
    }
  }

  /**
   * When a number explodes, we need to make sure that the root merges any values that
   * bubble up from the leave nodes to the root branch.
   * This also is used to propagate values from left to right and right to left
   *
   * @param snailFishNumber
   * @param mergedValue
   * @return
   */
  private[model] def mergeExploded(snailFishNumber: SnailFishNumber | Int,
                                   mergedValue: Int): SnailFishNumber | Int = {
    snailFishNumber match {
      case i: Int => i + mergedValue
      case sfn: SnailFishNumber =>
        if (mergedValue == 0) {
          sfn
        } else {
          val updatedLeft: SnailFishNumber | Int = mergeExploded(sfn.left, mergedValue)
          val updatedRight = if (updatedLeft == sfn.left) {
            mergeExploded(sfn.right, mergedValue)
          } else {
            sfn.right
          }

          SnailFishNumber(updatedLeft, updatedRight)
        }
    }
  }


  /**
   * This is a helper method that is used to propogate values. This should always
   * be zero unless we make it to the fourth level of the tree.
   * We need to take in to see if the original went from SnailFishNumber to Int.
   * This tells us that it exploded. If it exploded we want to keep the original number
   */
  private[model] def bubbleUpValue(snailFishNumber: SnailFishNumber | Int,
                                   originalSnailFishNumber: SnailFishNumber | Int,
                                   value: Int): (SnailFishNumber | Int, Int) = {
    val isChildExplode = originalSnailFishNumber match {
      case i: Int => false
      case sfn: SnailFishNumber => true
    }

    snailFishNumber match {
      case i: Int => if(isChildExplode) (i, value) else  (i + value, 0)
      case sfn: SnailFishNumber => (sfn, value)
    }
  }

  /**
   * TODO: This method is longer than I like my method definitions.
   * The shared Union Type does make this more verbose than I would like. I don't like methods that are over
   * 30 lines of code. This method is almost exactly 30 lines of code. Each case statement can be broken
   * up into it's own function.
   *
   * The exit condition is to first check if the search depth is at 5 (e.g. nested 4 times). If we are,
   * then the number returned needs to be 0, and it's left and right values bubble upwards. That's why
   * the return signature is ugly. the (Int, Int) values are used for parent nodes to know whether or
   * not to add those values.  If the depth of 5 is a SnailFishNumber, it needs to become 0
   * One thing I don't like is the whole Int, (Int, Int) results. This could be done in a
   * typed case class. Keeping for now
   *
   * TODO We only do this on a given branch. So we start start at depth 2
   * @param snailFishNumber
   * @param depth
   * @return
   */
  private[model] def traverseExplodeSplit(snailFishNumber: SnailFishNumber | Int,
                                          shouldExplode: Boolean,
                                          depth: Int = 2): (SnailFishNumber | Int, (Int, Int)) = {
    if (depth == 5 && shouldExplode) {
      snailFishNumber match {
        case i: Int => (i, (0, 0))
        case sfn: SnailFishNumber =>
          val left = snailFishNumberAsInt(sfn.left)
          val right = snailFishNumberAsInt(sfn.right)
          (0, (left, right))
      }
    } else {
      snailFishNumber match {
        case i: Int =>
          val num: SnailFishNumber | Int = if (i >= 10) split(i) else i
          (num, (0, 0))
    // TODO SOMEWHERE HERE WE aren't actually bubbling up the addition :Thinking: on the right hand side.
        case sfn: SnailFishNumber =>
          val left: (SnailFishNumber | Int, (Int, Int)) =
            traverseExplodeSplit(sfn.left, shouldExplode, depth + 1)
            
          val right: (SnailFishNumber | Int, (Int, Int)) =
            traverseExplodeSplit(sfn.right, shouldExplode, depth + 1)

          val propogateUp: (Int, Int) = tupleToPropogate(left._2, right._2)

          val finalLeft: (SnailFishNumber | Int, Int) = bubbleUpValue(left._1,  sfn.left, propogateUp._1)
          val finalRight: (SnailFishNumber | Int, Int) = bubbleUpValue(right._1,  sfn.right, propogateUp._2)
          val finalBubbleUp = (finalLeft._2, finalRight._2)

          (SnailFishNumber(finalLeft._1, finalRight._1), finalBubbleUp)
      }
    }
  }



  /**
   * This converts the final a snailfish number to integer. This is specifically for when we are at depth 4
   * where we know snailfish number will left and right are Ints
   * @param snailFishNumber
   * @return: SnailFishNumber as Int
  */
  private[model] def snailFishNumberAsInt(snailFishNumber: SnailFishNumber | Int): Int = {
    snailFishNumber match {
      case i: Int => i
      case _      => throw new Exception("Error you should only convert if you know the SnailFishNumber contains two ints")
    }
  }

  private[model] def tupleToPropogate(left: (Int, Int), right: (Int, Int)): (Int, Int) = {
    if (tupleSum(left) >= tupleSum(right)) {
      left
    } else {
      right
    }
  }


  private[model] def tupleSum(in: (Int, Int)): Int = {
    in._1 + in._2
  }

  /**
   * the left element of the pair should be the regular number divided by two and rounded down, while the right element
   * of the pair should be the regular number divided by two and rounded up. For example, 10 becomes [5,5], 11 becomes
   * [5,6], 12 becomes [6,6], and so on.
   * @param i input value that needs to be split.
   * @return SnailValue of the new split.
   */
  private[model] def split(i: Int): SnailFishNumber = {
    val left  = Math.floor(i / 2.0).toInt
    val right = Math.ceil(i / 2.0).toInt

    SnailFishNumber(left, right)
  }

  private[model] def maxNumberInTree(snailFishNumber: SnailFishNumber | Int,
                                     max: Int = Integer.MIN_VALUE): Int = {
    snailFishNumber match {
      case i:               Int => Math.max(i, max)
      case sfn: SnailFishNumber => Math.max(maxNumberInTree(sfn.left), maxNumberInTree(sfn.right))
    }
  }

  /**
   * Recurse through the SnailFishNumber Check the max depth.
   * @param snailFishNumber
   * @param depth
   * @return
   */
  def findDepth(snailFishNumber: SnailFishNumber | Int, depth: Int = 0): Int = {
    snailFishNumber match {
      case i: Int => depth
      case sfn: SnailFishNumber => Math.max(
        findDepth(sfn.left, depth + 1), findDepth(sfn.right, depth + 1))
    }
  }

  /**
   * This goes through the the input string from left to right and creates a Stack of elements
   * The algorithm is going to iterate through the list appending each element to a stack. We
   * do this until we find the first ']'. Once it goes through that stack popping until the first
   * open bracket. It then turns the insides of that bracket into a SnailFishPair
   * After we have gone through the stack we need to reverse it and then collect only
   * SnailFishNumbers. This shouldn't actually drop any elements given that everything
   * should be a SnailFishNumber by the end, but it allows us to only return one value.
   * The list should be size 1.
   *
   * This could have been done so many ways, but I really wanted to practice with Union Types.
   * This was a lot of fun figuring out how they can be used.
   *
   * @param input
   * @return a final SnailFishNumber that itself is made up of SnailFishNumbers
   */
  private[model] def parsePairs(input: String): SnailFishNumber = {
    val emptyAccum = List.empty[Char | Int | SnailFishNumber]
    val result: List[Char | Int | SnailFishNumber] = input.foldLeft(emptyAccum) {
      case (accum, ch) =>
        if (ch != ']') {
          ch +: accum
        } else {
          val popTil: List[Char | Int | SnailFishNumber] = accum.takeWhile(untilOpenBracket)
          val parsedInnerBracket: SnailFishNumber = parseInnerBracket(popTil)
          parsedInnerBracket +: accum.drop(popTil.size + 1)
        }
      }

    result.reverse.collect {
      case s: SnailFishNumber => s
    }.head
  }

  /**
   * Utility method that takes in Char | Int | SnailFishNumber and checks whether or
   * not the value is an openBracket. This is used to take elements from a list
   * until this condiditon is reached.
   *
   * @param elem
   * @return Whether or not the elem is an opening bracket [
   */
  private[model] def untilOpenBracket(elem: Char | Int | SnailFishNumber): Boolean = {
    elem match {
      case c: Char => c != '['
      case _ => true
    }
  }

  /**
   * We shouldn't get to this method unless we are already in an inner pair. With the way we are iterating
   * through the string we should have a pair of numbers or a pair of pairs on the left or right.
   *
   * example: 6, 5     <- in this case both are Char because we haven't parsed them into a pair yet
   * example: 6,Pair(5, 4) <- In this case the right side is a pari and the left side is a Char
   * example: Pair(1, 6)  Pair(5,4) <- in this case they haven't been created as a pair yet but also
   * probably don't have the comma. Either way we can always worry about the head and last element of
   * the values that come in here. We should never come into an invalid operation, if we do we should
   * just fail completely out.
   * TODO: Validate that I'm doing this in the right order
   * @param innerBracket
   * @return Pair: this is a pair of either Pair, Pair -> Int, Pair -> Pair, Int -OR-> Int, Int
   *         We swap right and left at the end because these were added FIFO to the stack.
   */
  private[model] def parseInnerBracket(innerBracket: List[Char | Int | SnailFishNumber]): SnailFishNumber = {
    val left = parseIntCharPair(innerBracket.head)
    val right = parseIntCharPair(innerBracket.last)

    SnailFishNumber(right, left)
  }

  /**
   * Utility method that takes in either Char, Int, or SnailFishNumber and returns an Int or
   * SnailFishNumber. The primary purpose of this is to convert the Char into an Int.
   * I could have done this upstream and filtered out the commas above, but didn't
   * really give it much thought till later.
   *
   * @param in: Single input Char | Int or SnailfishNumber
   * @return if it's not a Char just return it's self as an identity else convert the char to Int
   */
  def parseIntCharPair(in: Char | Int | SnailFishNumber): Int | SnailFishNumber = {
    in match {
      case ch: Char            => ch.asDigit
      case v: Int              => v
      case sn: SnailFishNumber => sn
    }
  }

}
