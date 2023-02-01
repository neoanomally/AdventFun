package com.sandersme.advent.twentytwo.model

class CycleInstructionTest extends munit.FunSuite {
  val TEST_INPUT = """noop
                     |addx 3
                     |addx -5""".stripMargin.split("\n").toList

  val CYCLE_INSTRUCTIONS = CycleInstructions.parseInput(TEST_INPUT)


  test("Validate the method signalStrengthsFromInput") {
    val signalStrengths = CycleInstructions.signalsStrengthFromInput(TEST_INPUT, Set(1, 2, 3, 4, 5, 6))
    val expectedValues = Map(2 -> 1, 3 -> 1, 4 -> 4, 5 -> 4, 6 -> -1)

    assertEquals(signalStrengths, expectedValues)
  }

  test("Validate that the LargeInput has the signal strength for 6 key signals is 13140") {
    val signalStrengths = CycleInstructions
      .signalsStrengthFromInput(TestValues.CYCLE_LARGE_TEST_INPUT, Set(20, 60, 100, 140, 180, 220))

    val expectedSum = 13140
    val sumOfStrengths = signalStrengths
      .map{ case(key, value) =>
        key * value
      }.sum

    assertEquals(sumOfStrengths, expectedSum)
  }

  test("Parse test input and validate everything maps correctly") {


    val expectedInstructions = List(
      CycleInstructions.parseInstruction("noop"),
      CycleInstructions.parseInstruction("addx 3"),
      CycleInstructions.parseInstruction("addx -5")
    )

    val secondExpected = AddXInstruction(3)

    assertEquals(CYCLE_INSTRUCTIONS, expectedInstructions)
    assertEquals(CYCLE_INSTRUCTIONS(1), secondExpected)
  }

  test("Process instructions and then run the accumulations") {
    val processedInstructions =  CycleInstructions.processInstructions(CYCLE_INSTRUCTIONS)

    val expectedResutls = List(1, 1, 4, 4, -1)

    assertEquals(processedInstructions, expectedResutls)
  }

  test("Test larger input") {
    val largeCycleInstructions = CycleInstructions.parseInput(TestValues.CYCLE_LARGE_TEST_INPUT)
    val processedInstructions = CycleInstructions.processInstructions(largeCycleInstructions)

    val interestingCycles = Set(20, 60, 100, 140, 180, 220)
    val signalStrengths = CycleInstructions.generateSignalStrengths(processedInstructions, interestingCycles)


    val sumOfSignalStrengths = signalStrengths.sum

    val expectedSumSignalStrengths = 13140L

    assertEquals(sumOfSignalStrengths, expectedSumSignalStrengths)
  }

  test("Something") {
    val largeCycleInstructions = CycleInstructions.parseInput(TestValues.CYCLE_LARGE_TEST_INPUT)
    val processedInstructions = CycleInstructions.processInstructions(largeCycleInstructions)
    CycleInstructions.generatePixelLocations(processedInstructions)
  }
}
