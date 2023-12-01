package com.sandersme.advent.twentythree

import com.sandersme.advent.Input
import com.sandersme.advent.twentythree.model.TrebuchetCalibration

object  Trebuchet {
  def main(args: Array[String]): Unit = {
    val input = Input.readTwentyThreeFromResource("day1_input")
    val parsedCalibrationValues = TrebuchetCalibration.parseInput(input)

    val sum = TrebuchetCalibration.sumCalibrationNumbers(parsedCalibrationValues)

    println(s"The sum of the calibration values: ${sum}")

    val parsedCalibrartionValues2 = TrebuchetCalibration.parseInputTranslated(input)
    val sumPart2 = TrebuchetCalibration.sumCalibrationNumbers(parsedCalibrartionValues2)

    println(s"Part two the sum of calibration values: ${sumPart2}")
  }


}
