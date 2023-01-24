package com.sandersme.advent.twentytwo.model

class RopeInstructionsTest extends munit.FunSuite {
  val TEST_INPUT: List[String] = """R 4
                                   |U 4
                                   |L 3
                                   |D 1
                                   |R 4
                                   |D 1
                                   |L 5
                                   |R 2""".stripMargin
    .split("\n")
    .toList

  test("Validate that we can read in the testInput") {
    val instructions: RopeInstructions = RopeInstructions.parseInstructions(TEST_INPUT)
    val headInstruction = instructions.instructions.head
    val secondToLast = instructions.instructions(instructions.instructions.length - 2)
    val expectedHeadInstruction = RopeInstruction(Direction.R, 4)
    val expectedSecondLastInstruction = RopeInstruction(Direction.L, 5)
    assertEquals(headInstruction, expectedHeadInstruction)
    assertEquals(secondToLast, expectedSecondLastInstruction)
  }
}
