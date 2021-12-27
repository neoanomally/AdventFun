package com.sandersme.advent.model

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

  def magnitude: Long = {
    SnailFishNumber.calculateMagnitude(this)
  }

  def reduce: SnailFishNumber = {
    SnailFishNumber.recursiveReduce(this)
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
    SnailFishNumber.willExplodeFromRoot(this) || SnailFishNumber.willSplit(this)
  }
}

object SnailFishNumber {

  /**
   *  The magnitude of a pair is 3 times the magnitude of its left element plus 2 times the magnitude of its right
   *  element. The magnitude of a regular number is just that number.
   * @param snailFishNumber
   * @return
   */
  private[model] def calculateMagnitude(snailFishNumber: SnailFishNumber | Int): Long = {
    snailFishNumber match {
      case i: Int => i.toLong
      case sfn: SnailFishNumber =>
        (3L * calculateMagnitude(sfn.left)) + (2L * calculateMagnitude(sfn.right))
    }
  }


  /**
   * This is a nice helper function. Since the Container object can either be a SnailFishNumber
   * or an Int with a union type this just converts it to the final class that we expect.
   *
   * @param snailFishNumber
   * @return
   */
  private[model] def explode(snailFishNumber: SnailFishNumber): SnailFishNumber = {
    val explodedSnailFish = ExplodedSnailFish(snailFishNumber)
    explodeSnailFish(explodedSnailFish).snailFishNumber match {
      case sfn: SnailFishNumber => sfn
      case _ => throw new Exception("Should not get here. This method should only be called from the root.")
    }
  }

  /**
   * This class is what we use to carry state throughout the tree. We are using immutable datastructures,
   * so in order to know what and when to update different portions of the tree we need to know
   * if we should explod, have we exploded and if we do explode how much should be applied left
   * and right of the branch the explosion happens.
   *
   * This contianer was created due to the amount of information that we needed to carry
   * with us throughout the traversal. It was easier to throw more data into this
   * container to pass across all the function calls.
   */
  private[model] case class ExplodedSnailFish(snailFishNumber: SnailFishNumber | Int,
                                              shouldExplode: Boolean = true, hasExploded: Boolean = false,
                                              leftValue: Int = 0, rightValue: Int = 0)
  enum Branch {
    case Left, Right, None
  }

  /**
   * V2 of trying to go left to right using recursion to explode and update values
   * throughout the tree Structure. The hard part that I'm having to wrap my mind around
   * is updating left to right and then making sure things that are updated
   * from the right propogate back to the left without infinite recursion: This is the longest I would ever
   * allow a function
   *
   *
   * @param exploded This is an accumulator case class that we are using to propogate lots of parameters
   * @param step
   * @param valueToAdd
   * @return
   */
  private[model] def explodeSnailFish(exploded: ExplodedSnailFish,
                                      step: Int = 1,
                                      branch: Branch = Branch.None): ExplodedSnailFish = {
    if (step == 5 && exploded.shouldExplode) {
      exploded.snailFishNumber match {
        case i: Int               => exploded
        case sfn: SnailFishNumber =>
          ExplodedSnailFish(0, false, true, snailFishToInt(sfn.left), snailFishToInt(sfn.right))
      }
    } else if (exploded.shouldExplode) {
      exploded.snailFishNumber match {
        case i: Int         => exploded
        case sfn: SnailFishNumber =>
          val isExplodeLeft = SnailFishNumber.willExplode(sfn.left, step + 1)

          if (isExplodeLeft) {
            val leftExploded =  explodeSnailFish(exploded.copy(sfn.left), step + 1)
            val pushRight = explodeSnailFish(leftExploded.copy(sfn.right), step + 1, Branch.Right) // HasExploded

            pushRight.copy(
              snailFishNumber = SnailFishNumber(leftExploded.snailFishNumber, pushRight.snailFishNumber))
          } else {
            val rightExploded = explodeSnailFish(exploded.copy(sfn.right), step + 1)
            val pushLeft = explodeSnailFish(rightExploded.copy(sfn.left), step + 1, Branch.Left)

            pushLeft.copy(
              snailFishNumber = SnailFishNumber(pushLeft.snailFishNumber, rightExploded.snailFishNumber))
          }
      }
    } else { // This is the case where it hasExploded.
      processHasExploded(exploded, step, branch)
    }
  }

  /**
   * This is to break out the functionality from above into it's own function. This *COULD* be tested
   * independentally, but I'm not going to. This method assumes that the explosion has occurred.
   * The State of the Explosion is propagated through the ExploredSnailFish class...
   * This class was mostly created due to how much information we need to propogate information left and
   * right. The branching strategy tells us which values we need to use and zero out. The state
   * needs to propogate throughout the tree until it is applied to one of the branches.
   *
   * This does not mutate any of the fish instead it updates each fish state with the previous object
   * passing along the new object the whole time. This does complicate things and sometimes state would probably be better
   *
   * @param exploded: This is a container for the state of how much and which branches to add values to
   * @param step Step is kind of a misnomer I propogated it throughout the codebase but it means depth.
   * @param branch WHich branch we need to apply. For example if we know that the snailfish to our right exploded
   *               we want to tell the branches to the left to apply the left state.
   * @return  A new copy of the exploded snailfish state which includes the new immutable SnailFishNumber
   *          at a specific branch. How much we apply to left and right branch.
   */
  private[model] def processHasExploded(exploded: ExplodedSnailFish, step: Int, branch: Branch): ExplodedSnailFish = {
    branch match {
      case Branch.Left =>
        exploded.snailFishNumber match
          case i: Int => exploded.copy(snailFishNumber = i + exploded.leftValue, leftValue = 0)
          case sfn: SnailFishNumber =>
            val right = explodeSnailFish(exploded.copy(sfn.right), step + 1, branch)
            val left = explodeSnailFish(right.copy(sfn.left), step + 1, branch)

            left.copy(snailFishNumber = SnailFishNumber(left.snailFishNumber, right.snailFishNumber))

      case Branch.Right =>
        exploded.snailFishNumber match
          case i: Int => exploded.copy(snailFishNumber = i + exploded.rightValue, rightValue = 0)
          case sfn: SnailFishNumber =>
            val left = explodeSnailFish(exploded.copy(sfn.left), step + 1, branch)
            val right = explodeSnailFish(left.copy(sfn.right), step + 1, branch)

            right.copy(snailFishNumber = SnailFishNumber(left.snailFishNumber, right.snailFishNumber))
      case Branch.None =>
        throw new Exception("Fatal Error, we should never enter this branch troubleshooting")
    }
  }

  private[model] def willExplode(snailFishNumber: SnailFishNumber | Int, depth: Int): Boolean = {
    snailFishNumber match {
      case i: Int => depth >= 6
      case snailFishNumber: SnailFishNumber =>
        willExplode(snailFishNumber.left, depth + 1) || willExplode(snailFishNumber.right, depth + 1)
    }
  }


  /**
   * The following is syntactic sugar for the following two functions. One of them does a recursion
   * splitting out the left most node.
   *
   * @param snailFishNumber: SHould be the root node in the branch.
   * @return New Root Node. Todo MOve this function to the case class
   */
  def split(snailFishNumber: SnailFishNumber | Int): SnailFishNumber = {
    splitSnailFish(snailFishNumber)._1 match {
      case sfn: SnailFishNumber => sfn
      case _ => throw new Exception("This should only be called from the root. Should not get here.")
    }
  }

  /** recurse through the function and look for values that may need to split. This step should only happen
   * after there are no explosions. only the firstMost should split
   * This always goes from left to right and only does one split at a time.
   * */
  private[model] def splitSnailFish(snailFishNumber: SnailFishNumber | Int,
                     shouldSplit: Boolean = true): (SnailFishNumber | Int, Boolean) = {
    if (shouldSplit) {
      snailFishNumber match {
        case i: Int =>
          if (i >= 10)
            (splitNumber(i), false) // This is the exit condition that sets all right nodes to know no splitting should happen
          else
            (i, shouldSplit)
        case sfn: SnailFishNumber =>
          val (left, continueSplitLeft) = splitSnailFish(sfn.left, shouldSplit)
          val (right, continueSplitRight) = splitSnailFish(sfn.right, continueSplitLeft)

          (SnailFishNumber(left, right), continueSplitRight)
      }
    } else { // Quickly just exit with the original number since we should split at most one per split
      (snailFishNumber, shouldSplit)
    }
  }

  /**
   * the left element of the pair should be the regular number divided by two and rounded down, while the right element
   * of the pair should be the regular number divided by two and rounded up. For example, 10 becomes [5,5], 11 becomes
   * [5,6], 12 becomes [6,6], and so on.
   * @param i input value that needs to be split.
   * @return SnailValue of the new split.
   */
  private[model] def splitNumber(i: Int): SnailFishNumber = {
    val left  = Math.floor(i / 2.0).toInt
    val right = Math.ceil(i / 2.0).toInt

    SnailFishNumber(left, right)
  }

  /**
   * This will continue to reduce until we have reduced as much as possible.
   *
   * @param snailFishNumbers
   * @return
   */
  @tailrec
  def recursiveReduce(snailFishNumber: SnailFishNumber): SnailFishNumber = {
    if (SnailFishNumber.willExplode(snailFishNumber, 1)) {
      recursiveReduce(SnailFishNumber.explode(snailFishNumber))
    } else if (SnailFishNumber.willSplit(snailFishNumber)) {
      recursiveReduce(SnailFishNumber.split(snailFishNumber))
    } else {
      snailFishNumber
    }
  }

  def parseInput(input: List[String]): SnailFishNumber = {
    val allPairs = input.map(parsePairs).map(_.reduce)

    val finalResult = allPairs.tail
      .foldLeft(allPairs.head){case (snailFishAccum, snailFish) =>
        SnailFishNumber(snailFishAccum, snailFish).reduce
      }

    finalResult
  }

  def parseInputPairCombinations(input: List[String]): List[SnailFishNumber] = {
    val listCombinations = for {
      left <- input
      right <- input

      if (left != right)
    } yield List(List(left, right), List(right, left))

    val flatList = listCombinations
      .flatten

      flatList
      .map(parseInput)
  }

  /** Any branch from the root tree that is 4 deep should explode. */
  private[model] def willExplodeFromRoot(snailFishNumber: SnailFishNumber): Boolean = {
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
   * This converts the final a snailfish number to integer. This is specifically for when we are at depth 4
   * where we know snailfish number will left and right are Ints
   * @param snailFishNumber
   * @return: SnailFishNumber as Int
  */
  private[model] def snailFishToInt(snailFishNumber: SnailFishNumber | Int): Int = {
    snailFishNumber match {
      case i: Int => i
      case _      => throw new Exception("Error you should only convert if you know the SnailFishNumber contains two ints")
    }
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
