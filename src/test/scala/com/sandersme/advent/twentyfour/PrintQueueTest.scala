package com.sandersme.advent.twentyfour

class PrintQueueTest extends  munit.FunSuite {
  test("validate that the values sum up after sorting should equal 123") {
    val sum = TEST_QUEUE.sumChanged
    assertEquals(sum, 123)
  }


  test("validate that we can sum up all the data for part 1 should equal 143") { 
    val sum = TEST_QUEUE.sumNotChanged

    assertEquals(sum, 143)
  }
  
  test("Validate that we can sort the value using custom sorter.") {
    TEST_QUEUE.sortUpdate(TEST_QUEUE.updates(4))
  }

  test("Validate two lists with different elements pass") {
    val first = List(1, 2, 3, 4, 5)
    val second = List(5, 4, 3, 2, 1)
    val checkDifferentFalse = PrintQueue.isIdentical(first, second)
    val checkDifferentTrue = PrintQueue.isIdentical(first, first)
    assertEquals(checkDifferentFalse, false)
    assertEquals(checkDifferentTrue, true)
  }

  val TEST_INPUT = """47|53
      97|13
      97|61
      97|47
      75|29
      61|13
      75|53
      29|13
      97|29
      53|29
      61|53
      97|53
      61|29
      47|13
      75|47
      97|75
      47|61
      75|61
      47|29
      75|13
      53|13

      75,47,61,53,29
      97,61,53,29,13
      75,29,13
      75,97,47,61,53
      61,13,29
      97,13,75,29,47"""
        .stripMargin
        .split("\n")
        .toList
        .map(_.trim())

  val TEST_QUEUE = PrintQueue.parseInput(TEST_INPUT)
}
