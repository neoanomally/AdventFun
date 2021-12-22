package com.sandersme.advent.algorithm

import com.sandersme.advent.algorithm.LimitQueue

import scala.collection.mutable

class LimitQueue[A](limit: Int)  extends mutable.Queue[A] {
  override def enqueue(item: A): LimitQueue.this.type = {
    super.enqueue(item)

    if (this.size > limit)
      this.dequeue

    this
  }

  def isAtLimit: Boolean = size == limit
}

object LimitQueue {
  def apply[A](limit: Int, startingInput: List[A]): LimitQueue[A] = {
    val queue = LimitQueue[A](limit)

    if (startingInput.size > limit) {
      val onlyLimitItems = startingInput
        .drop(startingInput.size - limit)

      queue.enqueueAll(onlyLimitItems)
    } else {
      queue
    }
  }

  def apply[A](limit: Int): LimitQueue[A] = {
    if (limit < 0) throw new Exception("Error the limit must be a positive number")
    new LimitQueue[A](limit)
  }
}
