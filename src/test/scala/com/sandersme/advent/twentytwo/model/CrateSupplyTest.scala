package com.sandersme.advent.twentytwo.model

class CrateSupplyTest extends munit.FunSuite {
  val TEST_INPUT: List[String] = """    [D]
                    |[N] [C]
                    |[Z] [M] [P]
                    | 1   2   3
                    |
                    |move 1 from 2 to 1
                    |move 3 from 1 to 3
                    |move 2 from 2 to 1
                    |move 1 from 1 to 2""".stripMargin
    .split("\n")
    .toList

  test("Test that we can move stuff from stack 1 to stack 3") {
    val stack1 = CrateStack(List('a', 'b', 'c'))
    val stack2 = CrateStack(List.empty)
    val stack3 = CrateStack(List('d'))

    val moveInstruction = CrateInstruction(2, 1, 3)
    val crateSupply = CrateSupply(Seq(stack1, stack2, stack3), List(moveInstruction))

    val updatedCrateSupply = CrateSupply.moveForwardOneInstruction(crateSupply)

    val expectedStack1 = CrateStack(List('c'))
    val expectedStack3 = CrateStack(List('b', 'a', 'd'))

    val stack1AfterMove = updatedCrateSupply.stackSeq(0)
    val stack3AfterMove = updatedCrateSupply.stackSeq(2)

    assertEquals(stack1AfterMove, expectedStack1)
    assertEquals(stack3AfterMove, expectedStack3)
    assertEquals(updatedCrateSupply.allInstructions, List.empty[CrateInstruction])
  }


  test("read the top of each crate should be ad") {
    val stack1 = CrateStack(List('a', 'b', 'c'))
    val stack2 = CrateStack(List.empty)
    val stack3 = CrateStack(List('d'))

    val crateSupply = CrateSupply(Seq(stack1, stack2, stack3), List.empty)

    val readTopCrates: String = crateSupply.readTopCrates
    val expectedReadTopCrates = "ad"

    assertEquals(readTopCrates, expectedReadTopCrates)
    }

  test("parseLine  that is empty should return a sequence of empty sequence/empty strings") {
    val inputEmpty = ""
    val emptyParsedLine = CrateSupply.parseLine("")
    val expectedEmptyParsedLine = Seq.empty[Option[Char]]

    val inputEmptyBySpaces = "           "
    val emptySpacesLine = CrateSupply.parseLine(inputEmptyBySpaces)
    val expectedSpacesEmptyParsedLine = Seq.fill(3)(None)

    assertEquals(emptyParsedLine, expectedEmptyParsedLine)
    assertEquals(emptySpacesLine, expectedSpacesEmptyParsedLine)
  }

  test("parseLine includes 3 different letters with some spaces") {
    val testInput = "[T]     [D]         [L]"
    val parsedInputTDL = CrateSupply.parseLine(testInput)
    val expectedOutput = Seq(Option('T'), None, Option('D'), None, None, Option('L'))

    assertEquals(parsedInputTDL, expectedOutput)
  }

  test("parseAllStackedCrates up till the first empty line will read in lines and" +
    "separate stackedCrates from Moves validate the crates are stacked correctly") {
    val (stackedCrates, _) = CrateSupply.parseAllStackedCrates(TEST_INPUT)
    val expectedStackedCrates = List(
      CrateStack(List('N', 'Z')),
      CrateStack(List('D', 'C', 'M')),
      CrateStack(List('P'))
    )

    assertEquals(stackedCrates, expectedStackedCrates)
  }

  test("parseAllINstructions after the crateStacks have been created will have 4 instructions") {
    val (_, remainingLines) = CrateSupply.parseAllStackedCrates(TEST_INPUT)
    val moveInstructions = CrateSupply.parseAllInstructions(remainingLines)

    val expectedMoveInstructions = List(
      CrateInstruction(1, 2, 1),
      CrateInstruction(3, 1, 3),
      CrateInstruction(2, 2, 1),
      CrateInstruction(1, 1, 2)
    )

    assertEquals(moveInstructions, expectedMoveInstructions)
  }

  test("parseInput into both expected CrateSupply") {
    val crateSupply = CrateSupply.fromInputSeq(TEST_INPUT)

    val expectedStackedCrates = List(
      CrateStack(List('N', 'Z')),
      CrateStack(List('D', 'C', 'M')),
      CrateStack(List('P'))
    )

    val expectedMoveInstructions = List(
      CrateInstruction(1, 2, 1),
      CrateInstruction(3, 1, 3),
      CrateInstruction(2, 2, 1),
      CrateInstruction(1, 1, 2)
    )
    val expectedCrateSupply = CrateSupply(
      expectedStackedCrates,
      expectedMoveInstructions
    )

    assertEquals(crateSupply, expectedCrateSupply)
  }


  test("move forward all instructions") {
    val crateSupply = CrateSupply.fromInputSeq(TEST_INPUT)

    val finalCrateSupply = crateSupply.moveForwardAllInstructions
    val expectedFinalCrateStacks: Seq[CrateStack] = Seq(
      CrateStack(List('C')),
      CrateStack(List('M')),
      CrateStack(List('Z', 'N', 'D', 'P'))
    )


    assertEquals(finalCrateSupply.allInstructions, List.empty[CrateInstruction])
    assertEquals(finalCrateSupply.stackSeq, expectedFinalCrateStacks)
  }

  test("print off the top of the final stack should equal CMZ") {
    val crateSupply = CrateSupply.fromInputSeq(TEST_INPUT)

    val finalCrateSupply = crateSupply.moveForwardAllInstructions
    val topEachStackString = finalCrateSupply.printTopEachStack
    val expectedOutput = "CMZ"

    assertEquals(topEachStackString, expectedOutput)
  }
}
