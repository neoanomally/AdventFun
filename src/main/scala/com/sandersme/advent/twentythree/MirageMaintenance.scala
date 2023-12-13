package com.sandersme.advent.twentythree

import com.sandersme.advent.Input
import com.sandersme.advent.twentythree.model.OasisReport

object MirageMaintenance extends App {

  val input = Input.readTwentyThreeFromResource("day9_input")
  val oasisReport = OasisReport.parseInput(input)
  val sumOfNextValues = oasisReport.calculateSumOfNext
  val sumOfPreviousValues = oasisReport.calculateSumOfPrevious
  println(s"The sum of all the next values in the OasisReport is: ${sumOfNextValues}")
  
  println(s"The sum of all previous values in the OasisReport is: ${sumOfPreviousValues}")
}
