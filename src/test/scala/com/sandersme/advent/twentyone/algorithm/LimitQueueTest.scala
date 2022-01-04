package com.sandersme.advent.twentyone.algorithm

import munit.FunSuite

class LimitQueueTest extends FunSuite {

  test("Testing out a queue of size 5 is always 5") {
    val queueSizeFive = LimitQueue[Int](5)

    List.range(1, 10).foreach(queueSizeFive.enqueue)

    assertEquals(queueSizeFive.size, 5)

  }

  test("Limit Queue empty test") {
    val testQueue = LimitQueue[Int](3)

    assertEquals(testQueue.size, 0)
  }

  test("Limit queue with an input list larger than n") {
    val inputList: List[Int] = List(1, 2, 3, 4, 5, 6, 7)

    val queue = LimitQueue(3, inputList)

    assertEquals(queue.toList, inputList.drop(inputList.size - 3))
  }
}
