package com.sandersme.advent

/**
 * As the submarine drops below the surface of the ocean, it automatically performs
 * a sonar sweep of the nearby sea floor. On a small screen, the sonar sweep report
 * (your puzzle input) appears: each line is a measurement of the sea floor depth
 * as the sweep looks further and further away from the submarine.

 * For example, suppose you had the following report:
 * 199 200 208 210 200 207 240 269 260 263
 *
 * This report indicates that, scanning outward from the submarine, the sonar sweep found
 * depths of 199, 200, 208, 210, and so on.
 * The first order of business is to figure out how quickly the depth increases, just so you
 * know what you're dealing with - you never know if the keys will get carried into deeper water
 * by an ocean current or a fish or something.
 *To do this, count the number of times a depth measurement increases from the previous measurement.
 * (There is no measurement before the first measurement.) In the example above, the changes are as follows:


 */

object SonarSweepDayOne {

  trait BaseAccumulator {
    def update(depth: Int): BaseAccumulator
    val increments: Int
  }

  case class Accumulator(prevVal: Option[Int], increments: Int) extends BaseAccumulator {
    def update(depth: Int): Accumulator = {
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
                                      increments: Int) extends BaseAccumulator {

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

  val defaultAccumulator = Accumulator(None, 0)
  val defaultSlidingWindowAccumulator = SlidingWindowAccumulator(LimitQueue(3), 0)

  def main(args: Array[String]): Unit = {
    val lines = Input.readFromDataResource("day1_input")

    val depths = lines.map(_.toInt)

    // NOTE: This is just for sanity check. I realize I could do a sliding window
    // Using the sliding method on the list, but that doesn't let me play with
    // Functional FoldLeft ;) 
    val slidingWindow = calculateIncreases(
      depths.sliding(3)
        .map(_.sum)
        .toList, defaultAccumulator
    )


    val previousSoundings = calculateIncreases(depths, defaultAccumulator)
    val slidingWindowSoundings = calculateIncreases(depths, defaultSlidingWindowAccumulator)
    println(s"Reading previous value only $previousSoundings")
    println(s"Readings based on sliding window $slidingWindowSoundings")
    println(s"Sliding windows $slidingWindow")
  }

  def calculateIncreases(depths: List[Int], defaultAccum: BaseAccumulator): BaseAccumulator = {
    depths
      .foldLeft(defaultAccum) {
        (accum, depth) => accum.update(depth)
      }
  }
}
