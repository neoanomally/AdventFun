package com.sandersme.advent.twentyfour

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

    println("original output:  " + ChronoSpatialComputer.advanceStateTillComplete(computerTwo.copy(debug = true)))
    println("\n\n\n--------------------\n\n\n")
    println("Alternate output: " + ChronoSpatialComputer.advanceStateTillComplete(copiedWithAltInput.copy(debug = true)))
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

  val CHRONOSPATIAL_COMPUTER = ChronoSpatialComputer.parseInput(TEST_INPUT)
}
