package com.sandersme.advent

import com.sandersme.advent.algorithm.LimitQueue
import com.sandersme.advent.model.Accumulator.calculateIncreases
import com.sandersme.advent.model.{SingleAccumulator, SlidingWindowAccumulator}

/**
 * As the submarine drops below the surface of the ocean, it automatically performs
 * a sonar sweep of the nearby sea floor. On a small screen, the sonar sweep report
 * (your puzzle input) appears: each line is a measurement of the sea floor depth
 * as the sweep looks further and further away from the submarine.
 *
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
 *
 *
 */

object SonarSweep {
  val defaultAccumulator = SingleAccumulator(None, 0)
  val defaultSlidingWindowAccumulator = SlidingWindowAccumulator(LimitQueue(3), 0)

  val DAY_1_INPUT = "day1_input"

  def main(args: Array[String]): Unit = {
    val lines = Input.readFromDataResource(DAY_1_INPUT)

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

}
