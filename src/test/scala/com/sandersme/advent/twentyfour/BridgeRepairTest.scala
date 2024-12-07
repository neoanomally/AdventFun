package com.sandersme.advent.twentyfour

class BridgeRepairTest extends munit.FunSuite {
  test("Validate that we can parse instructions correctly") {
    val testInstructionInput = "190: 10 19"

    val expected = RepairInstruction(190, List(10, 19))
    val parsed = BridgeRepair.parseInstruction(testInstructionInput)

    assertEquals(parsed, expected)
  }

  test("RepairInstructions Test taht the first input canEqual Right and index two can't equal Right") {
    val indexZero = PARSED_INPUT.instructions(0).leftCanEqualRight
    val indexTwo = PARSED_INPUT.instructions(2).leftCanEqualRight

    assertEquals(indexZero, true)
    assertEquals(indexTwo, false)
  }

  test("BridgeRepair test that the sum of the values in the test input is equal to 3749") {
    val sum = PARSED_INPUT.sumCanEqual

    assertEquals(sum, 3749L)
  }

  test("Validate that we can move and concat 32 and 7900 should equal 327900") {
    val moved = BridgeRepair.moveConcat(32L, 7900L)

    assertEquals(moved, 327900L)
  }

  val TEST_INPUT = """190: 10 19
      3267: 81 40 27
      83: 17 5
      156: 15 6
      7290: 6 8 6 15
      161011: 16 10 13
      192: 17 8 14
      21037: 9 7 18 13
      292: 11 6 16 20"""
        .stripMargin
        .split("\n")
        .map(_.trim)
        .toList

  val PARSED_INPUT = BridgeRepair.parseInput(TEST_INPUT)
}
