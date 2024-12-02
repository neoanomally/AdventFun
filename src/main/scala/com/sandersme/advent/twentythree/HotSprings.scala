package com.sandersme.advent.twentythree

import com.sandersme.advent.Input
import com.sandersme.advent.twentythree.model.HotSpringsDiagnostics

object HotSprings extends App { 
  val input = Input.readTwentyThreeFromResource("day12_input")

  val diagnostics = HotSpringsDiagnostics.parseInput(input) 

  val sumOfPossibleArrangements = HotSpringsDiagnostics.countAllPossibleArrangements(diagnostics)

  println(s"The number of possible arrangements is: ${sumOfPossibleArrangements}")
   
  val unfoldedDiagnostics = diagnostics.map(_.unfold)
  // THE FOLLOWING Won't run
  // val sumOfUnfoldedArrangements = HotSpringsDiagnostics.countAllPossibleArrangements(unfoldedDiagnostics)
  // println(s" The number of possible unfolded Arranements is: ${sumOfUnfoldedArrangements}")
}
