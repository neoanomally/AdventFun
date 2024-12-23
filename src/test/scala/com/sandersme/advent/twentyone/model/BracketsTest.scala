package com.sandersme.advent.twentyone.model

import Brackets._
import scala.collection.mutable.Stack
class BracketsTest extends munit.FunSuite {

  val input: Array[String] = Array("[({(<(())[]>[[{[]{<()<>>",
                                "[(()[<>])]({[<{<<[]>>(",
                                "{([(<{}[<>[]}>{[]{[(<()>",
                                "(((({<>}<{<{<>}{[]{[]{}",
                                "[[<[([]))<([[{}[[()]]]",
                                "[{[{({}]{}}([{[{{{}}([]",
                                "{<[[]]>}<{[{[{[]{()[[[]",
                                "[<(<(<(<{}))><([]([]()",
                                "<{([([[(<>()){}]>(<<{{",
                                "<{([{{}}[<[[[<>{}]]]>[]]")

  val TEST_BRACKETS = Brackets.parseInput(input.toList)

  test("Validate I can parse the input") {
    val brackets: List[Brackets] = Brackets.parseInput(input.toList)
    val headBracket = brackets.head.values.head

    assertEquals(headBracket, Bracket.LeftSquare)
    assertEquals(brackets.length, 10)
  }

  test("Valdiate enum back and forth with a bracket type") {
    val bracket = Brackets.bracketFromValue('{')

    assertEquals(bracket, Bracket.LeftCurly)
  }

  test("Validate that we find the closing bracket for curly brace") {
    val bracket = Bracket.LeftCurly
    val closingBracket = bracket.findOppositeBracket

    assertEquals(closingBracket, Bracket.RightCurly)
  }

  test("Valdiate that there are opening and closing brackets") {
    val allOpeningBrackets = List(Bracket.LeftCurly, Bracket.LeftSquare,
      Bracket.LeftRound, Bracket.LeftAngle)

    val allClosingBrackets = List(Bracket.RightCurly, Bracket.RightSquare,
      Bracket.RightRound, Bracket.RightAngle)

    val mixed = allOpeningBrackets ++ allClosingBrackets

    assertEquals(allClosingBrackets.forall(_.isOpeningBracket), false)
    assertEquals(allOpeningBrackets.forall(_.isOpeningBracket), true)
    assertEquals(mixed.forall(_.isOpeningBracket), false)
  }

  test("Value Of Will throw exception on bad input") {
    intercept[Exception] {
      val c = Brackets.bracketFromValue('c')
    }
  }

  test("Check the error code validator works") {
    val validatorIncomplete = Brackets.validateErrorCodes(TEST_BRACKETS.head)
    val validatorCorrupted = Brackets.validateErrorCodes(TEST_BRACKETS(2))

    assertEquals(validatorIncomplete.stoppedBracket, None)
    assertEquals(validatorCorrupted.stoppedBracket, Some(Bracket.RightCurly))
  }

  // TODO: Hey, you add this to a miain method somewhere :D
  test("Calculate the total from the error codes") {
    val score = Brackets.calculateCorruptedScore(TEST_BRACKETS)

    assertEquals(score, 26397)
  }

  test("Calculate the completed codes for incomplete error codes".ignore) {
    import Bracket._
    val completedErrorCodes = Brackets.calculateIncompleteScores(TEST_BRACKETS)

    val headExpected = CompletedErrorCodes(
      List(RightCurly, RightCurly, RightSquare, RightSquare,
        RightRound, RightCurly, RightRound, RightSquare), 288957
    )

    assertEquals(completedErrorCodes.head, headExpected)
  }

  test("Complete Median Scores from incompleted error codes".ignore) {
    val completedErrorCodes = Brackets.calculateIncompleteScores(TEST_BRACKETS)

    val medianScore = calculateMiddleScore(completedErrorCodes)

    assertEquals(medianScore, 288957L)
  }
}
