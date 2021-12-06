package com.sandersme.advent

import com.sandersme.advent.model.BinaryCoding
import com.sandersme.advent.model.BinaryCoding._
import com.sandersme.advent.model.Diagnostics
import com.sandersme.advent.model.Diagnostics._

object BinaryDiagnostic {
  /**
   * The submarine has been making some odd creaking noises, so you ask it to produce a diagnostic report
   * just in case.
   * The diagnostic report (your puzzle input) consists of a list of binary numbers which, when decoded properly,
   * can tell you many useful things about the conditions of the submarine. The first parameter to check is the power
   * consumption.
   * You need to use the binary numbers in the diagnostic report to generate two new binary numbers (called the gamma
   * rate and the epsilon rate). The power consumption can then be found by multiplying the gamma rate
   * by the epsilon rate.
   * Each bit in the gamma rate can be determined by finding the most common bit in the corresponding position of
   * all numbers in the diagnostic report. For example, given the following diagnostic report:
   *
   * Considering only the first bit of each number, there are five 0 bits and seven 1 bits. Since the most common bit is 1, the first bit of the gamma rate is 1.
   * The most common second bit of the numbers in the diagnostic report is 0, so the second bit of the gamma rate is 0.
   * The most common value of the third, fourth, and fifth bits are 1, 1, and 0, respectively, and so the
   * final three bits of the gamma rate are 110.
   * So, the gamma rate is the binary number 10110, or 22 in decimal.
   * The epsilon rate is calculated in a similar way; rather than use the most common bit, the least common bit
   * from each position is used. So, the epsilon rate is 01001, or 9 in decimal. Multiplying the gamma rate (22) by
   * the epsilon rate (9) produces the power consumption, 198.
   * Use the binary numbers in your diagnostic report to calculate the gamma rate and epsilon rate, then multiply
   * them together. What is the power consumption of the submarine? (Be sure to represent your answer in
   * decimal, not binary.)
   *
   * ---------- PART TWO ------------------
   * Next, you should verify the life support rating, which can be determined by multiplying the oxygen
   * generator rating by the CO2 scrubber rating.
   * Both the oxygen generator rating and the CO2 scrubber rating are values that can be found in your diagnostic
   * report - finding them is the tricky part. Both values are located using a similar process that involves filtering
   * out values until only one remains. Before searching for either rating value, start with the full list of binary
   * numbers from your diagnostic report and consider just the first bit of those numbers. Then:
   * Keep only numbers selected by the bit criteria for the type of rating value for which you are searching.
   * Discard numbers which do not match the bit criteria.
   * If you only have one number left, stop; this is the rating value for which you are searching.
   * Otherwise, repeat the process, considering the next bit to the right.
   * The bit criteria depends on which type of rating value you want to find:

To find oxygen generator rating, determine the most common value (0 or 1) in the current bit position, and keep only numbers with that bit in that position. If 0 and 1 are equally common, keep values with a 1 in the position being considered.
To find CO2 scrubber rating, determine the least common value (0 or 1) in the current bit position, and keep only numbers with that bit in that position. If 0 and 1 are equally common, keep values with a 0 in the position being considered.
   */
  def main(args: Array[String]): Unit = {

    val binaryInput = Input.readFromDataResource("day3_input")

    val diagnostics: Diagnostics = Diagnostics.fromBinaryInput(binaryInput)

    val gammRate = diagnostics.gammaRate
    val epsilonRate = diagnostics.epsilonRate
    val powerConsumption = diagnostics.powerConsumption
    val oxygenRating = diagnostics.oxygenGeneratorRating
    val co2ScrubberRating = diagnostics.co2ScrubberRating
    val lifeSupportRating = diagnostics.lifeSupportRating

    println(s"Here is our bit counters $diagnostics")
    println(s"Gamma Rate: $gammRate")
    println(s"Episolon Rate: $epsilonRate")
    println(s"Power consumption $powerConsumption")
    println(s"Oxygen Generator Rating: $oxygenRating")
    println(s"CO2 Scrubber Rating: $co2ScrubberRating")
    println(s"Life Support Rating: $lifeSupportRating")
  }
}
