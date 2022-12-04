package com.sandersme.advent.twentytwo.model



class RockPaperScissorsTest extends munit.FunSuite {

  test("Test that the expected handshape parses correctly") {
    val first = ExpectedHandShapes.partOneParseFromInputLine("A X")
    val second = ExpectedHandShapes.partOneParseFromInputLine("C X")
    val third = ExpectedHandShapes.partOneParseFromInputLine("B Z")

    val expectedFirst = ExpectedHandShapes(HandShape.Rock, HandShape.Rock)
    val expectedSecond = ExpectedHandShapes(HandShape.Scissors, HandShape.Rock)
    val expectedThird = ExpectedHandShapes(HandShape.Paper, HandShape.Scissors)

    assertEquals(first, expectedFirst)
    assertEquals(second, expectedSecond)
    assertEquals(third, expectedThird)
  }

  test("Expected Input to throw an exception as it's not in the HandShape Encoder") {
    intercept[Exception] {
      val first = ExpectedHandShapes.partOneParseFromInputLine("R X")
    }
  }

  test("Expected Input to throw an AssertionError as it's an empty line") {
    intercept[AssertionError] {
      val first = ExpectedHandShapes.partOneParseFromInputLine("    ")
    }
  }

  test("Using test input assert the sequence of scores should be 8, 1, 6") {
    val TEST_INPUT = List("A Y", "B X", "C Z")

    val scores = RockPaperScissors.partOneScoresFromInput(TEST_INPUT)
    val expectedScores: Seq[Long] = Seq(8L, 1L, 6L)

    assertEquals(scores, expectedScores)
  }

  test("Scores from test input should sum to 15") {
    val TEST_INPUT = List("A Y", "B X", "C Z")
    val sumOfScores = RockPaperScissors.partOneTotalScoreFromInput(TEST_INPUT)

    assertEquals(sumOfScores, 15L)
  }

  test("SCore from TEST Input for part two should sum to 12") {
    val TEST_INPUT = List("A Y", "B X", "C Z")
    val sumOfScores = RockPaperScissors.partTwoTotalScoreFromInput(TEST_INPUT)

    assertEquals(sumOfScores, 12L)
  }
}
