package com.sandersme.advent.twentyfour

import com.sandersme.advent.twentyfour.ChronoSpatialComputer.solve
import com.sandersme.advent.twentyfour.ChronoSpatialComputer.runOneStep
import com.sandersme.advent.twentyfour.ChronoSpatialComputer.solveForAll
import com.sandersme.advent.twentyfour.ChronoSpatialComputer.candidatesForA
import com.sandersme.advent.twentyfour.ChronoSpatialComputer.solveValuesStep

class ChronoSpatialComputerTest extends munit.FunSuite {
  
  test("TESTING Parse") {
    val expectedInstructions = List( 0,1,5,4,3,0)
    val expectedRegisters = Register(729, 0, 0)
      
    assertEquals(CHRONOSPATIAL_COMPUTER.instructions, expectedInstructions)
    assertEquals(CHRONOSPATIAL_COMPUTER.registers, expectedRegisters)
  }

  test("Validate we can advance state") {
    val expectedOutput = List(4,6,3,5,6,3,5,2,1,0).mkString(",")

    val updated = ChronoSpatialComputer.advanceStateTillComplete(CHRONOSPATIAL_COMPUTER)

    assertEquals(updated.printOutput, expectedOutput)
  }

  test("TEST TWO: ") {
    val computerTwo = ChronoSpatialComputer.parseInput(TEST_INPUT_TWO)
    val altRegister = computerTwo.registers.copy(A = 117440)
    val copiedWithAltInput = computerTwo.copy(registers =  altRegister)
  }

  test("Test if I can find candidates at a step".ignore) {
    val solution = ChronoSpatialComputer.parseInput(TEST_INPUT_TWO)
   //// TODO There can be multiple potential soltuions solve for all 
    println("OUTPUT SOLUTION: "  + ChronoSpatialComputer.advanceStateTillComplete(solution).printOutput)
    val firstCandidate = 5
    val A = 0 // we know the final A is between 0 - 7 for the program to exit
    println((solveForAll(List(6, 2, 7, 2, 3, 1, 6, 0, 5)).mkString(",")) + " should have final register: " + 47006051)
    println("LOOKING FOR SOLUTION TO: " + solveForAll(List(2,4,1,3,7,5,1,5,0,3,4,3,5,5,3,0)))
///  2,4,1,3,7,5,1,5,0,3,4,3,5,5,3,0 <- My actual program
// 0,3,5,4,3,0

  }

  test("Teset to see if the input Three outputs debug".ignore) {
    val production = ChronoSpatialComputer.parseInput(TEST_THREE_INPUT)
    testSolve(production.registers.A)
  }

  def testSolve(in: Int): Unit = {
    var A = in
    var B = 0 
    var C = 0

    while (A != 0) {                  // 3, 0 
      B = A % 8                       // (2, 4)
      B = B ^ 3                       // 1, 3
      C = (A / Math.pow(2, B)).toInt  // 7, 5 -- C = A / Math.pow(2, B)
      B = B ^ 5                       // 1, 5
      A = A / 8                       // 1, 5 --
      B = B ^ C   
      println("A: " + A + " with output expecting: " + (B % 8))
    }
  }

  


  test("Testing to see if the input of B, C Matter".ignore) {
    println(runOneStep(155, 3, 1804233))
    println(runOneStep(155, 1314143, 232))
    println(runOneStep(155, 3, 553))
    println(runOneStep(155, 5, 18033))
    println(runOneStep(155, 98, 18033))
  }
  // 117440 -- 0,3,5,4,3,0
  // 

  val TEST_INPUT = """Register A: 729
    Register B: 0
    Register C: 0

    Program: 0,1,5,4,3,0""".stripMargin
  .split("\n")
  .toList
  .map(_.trim)

  val TEST_INPUT_TWO = """Register A: 2024
    Register B: 0
    Register C: 0

    Program: 0,3,5,4,3,0""".stripMargin // Interestingly I need to figure out how I can recreate this, and we can start without
  .split("\n")
  .toList
  .map(_.trim)

  val TEST_THREE_INPUT = """Register A: 47006051
Register B: 0
Register C: 0

Program: 2,4,1,3,7,5,1,5,0,3,4,3,5,5,3,0""".stripMargin.split("\n").toList.map(_.trim)

  val CHRONOSPATIAL_COMPUTER = ChronoSpatialComputer.parseInput(TEST_INPUT)
}
