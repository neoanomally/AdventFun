package com.sandersme.advent.twentytwo.model

case class CrateStack(stack: List[Char])

object CrateStack {
  /**
   * Take the original stack and pop off the top off one at a time into a new stack. These
   * will be used to put on the top of another stack.
   * @param originalStack
   * @param n
   * @return (Stack, Stack). The first stack si the items that will be moved to another stack.
   *         The second stack will replace the original stack.
   */
  def splitMoveTopN(originalStack: CrateStack, n: Int): (CrateStack, CrateStack) = {
    val itemsPopped = originalStack
      .stack
      .take(n)
      .reverse

    val remainingStack = originalStack.stack.drop(n)

    (CrateStack(itemsPopped), CrateStack(remainingStack))
  }
}