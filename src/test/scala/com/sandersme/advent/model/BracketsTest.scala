package com.sandersme.advent.model

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
    val score = Brackets.calculateScore(TEST_BRACKETS)

    assertEquals(score, 26397)

    import com.sandersme.advent.Input
    val input = Input.readFromDataResource("day10_input")
    val bracketsInput = Brackets.parseInput(input)
    val inputScore = Brackets.calculateScore(bracketsInput)

    println(s"input score: $inputScore")
  }
}
