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

  case class Accumulator(prevVal: Option[Int], sumOfIncreases: Int)
  val defaultAccumulator = Accumulator(None, 0)

  def main(args: Array[String]): Unit = {
    val lines = Input.readFromDataResource("day1_input")

    val depths = lines.map(_.toInt)

    println(calculateNumberOfIncreases(depths))
  }

  def calculateNumberOfIncreases(depths: List[Int]): Accumulator = {
    depths
      .foldLeft(defaultAccumulator) { (accum, depth) =>
        // TODO: Might be nice to clean this up, but these are the three cases. Could use maps
        if (accum.prevVal.isEmpty) {
          accum.copy(prevVal = Some(depth))
        } else if (depth > accum.prevVal.get) {
          accum.copy(prevVal = Some(depth), sumOfIncreases = accum.sumOfIncreases + 1)
        } else {
          accum.copy(prevVal = Some(depth))
        }
      }
  }
}
