package com.sandersme.advent.twentytwo.model

class CrateStackTest extends munit.FunSuite {

  test("Check to make sure that the popped stack is in the correct response") {

    val originalStack = CrateStack(List('c', 'b', 'r', 'g', 'k'))

    val (itemsToMove, updatedCrateStack) = CrateStack.splitMoveTopN(originalStack, 3)

    val expectedItemsToMove = CrateStack(List('r', 'b', 'c'))
    val expectedUpdatedCrateStack = CrateStack(List('g', 'k'))

    assertEquals(itemsToMove, expectedItemsToMove)
    assertEquals(updatedCrateStack, expectedUpdatedCrateStack)
  }

}
