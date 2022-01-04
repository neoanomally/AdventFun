package com.sandersme.advent.twentyone.model

import com.sandersme.advent.twentyone.algorithm.LimitQueue

trait Accumulator {
  def update(depth: Int): Accumulator
  val increments: Int
}

object Accumulator {
  def calculateIncreases(depths: List[Int], defaultAccum: Accumulator): Accumulator = {
    depths
      .foldLeft(defaultAccum) {
        (accum, depth) => accum.update(depth)
      }
  }
}

case class ProbeAccum(inBounds: Boolean = false, x: Int = 0, step: Int = 0)

case class SingleAccumulator(prevVal: Option[Int], increments: Int) extends Accumulator {
  def update(depth: Int): SingleAccumulator = {
    // TODO: Might be nice to clean this up, but these are the three cases. Could use maps
    if (prevVal.isEmpty) {
      copy(prevVal = Some(depth))
    } else if (depth > prevVal.get) {
      copy(prevVal = Some(depth), increments = increments + 1)
    } else {
      copy(prevVal = Some(depth))
    }
  }
}

case class SlidingWindowAccumulator(queue: LimitQueue[Int],
                                    increments: Int) extends Accumulator {

  def update(nextVal: Int): SlidingWindowAccumulator = {
    val wasAtLimit = queue.isAtLimit
    val previousSum = queue.sum
    val updatedQueue = queue.enqueue(nextVal)
    val currentSum = updatedQueue.sum

    val hasIncreased = currentSum > previousSum

    if (wasAtLimit && hasIncreased) {
      copy(queue = updatedQueue, increments = increments + 1)
    } else {
      copy(queue = updatedQueue, increments = increments)
    }
  }
}