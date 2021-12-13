package com.sandersme.advent.model

import Brackets.Bracket.*
import Brackets.Bracket

import scala.collection.mutable.Stack

case class Brackets(values: List[Bracket]) // todo we will use some other datastructure

object Brackets {
  case class BracketAccumulator(bracketStack: Stack[Bracket], stoppedBracket: Option[Bracket])

  enum Bracket(val typeOf: Char) {
    case LeftAngle extends Bracket('<')
    case RightAngle extends Bracket('>')
    case LeftSquare extends Bracket('[')
    case RightSquare extends Bracket(']')
    case LeftRound extends Bracket('(')
    case RightRound extends Bracket(')')
    case LeftCurly extends Bracket('{')
    case RightCurly extends Bracket('}')


    override def toString: String = typeOf.toString
  }

  // Realistically I'd do HeadOption here and handle it elsewhere, but not here. No nOt here!
  def bracketFromValue(in: Char): Bracket = {
    Bracket.values
      .filter(bracket => bracket.typeOf == in)
      .head
  }

  extension (bracket: Bracket) {
    def findOppositeBracket: Bracket = bracket match {
      case LeftCurly => RightCurly
      case LeftAngle => RightAngle
      case LeftSquare => RightSquare
      case LeftRound => RightRound
      case RightCurly => LeftCurly
      case RightSquare => LeftSquare
      case RightRound => LeftRound
      case RightAngle => LeftAngle
    }

    def isOpeningBracket: Boolean = bracket match {
      case LeftCurly | LeftAngle | LeftSquare | LeftRound => true
      case _ => false
    }

    def isClosingBracket: Boolean = !isOpeningBracket
  }


  // WE just need to stop at the first incorrect closing character.
  // What we can do is have a map that keeps track of the number of opening
  // brackets and decrement when we have a closing bracket, but we can
  // only decrement when we have > 0 brackets
  def validateErrorCodes(errorCodes: Brackets): BracketAccumulator = {

    // Populate a Map with all the leftBrackets
    val bracketCounter = Stack[Bracket]()

    val emptyAccumulator = BracketAccumulator(bracketCounter, None)

    errorCodes.values.foldLeft(emptyAccumulator) { case (bracketAccumulator, bracket) =>
      if (bracketAccumulator.stoppedBracket.isEmpty) {
        accumulate(bracketAccumulator, bracket)
      } else {
        bracketAccumulator
      }
    }
  }

  // TODO some ugly nested

  /**
   * We have several different conditions:
   * 1. If the bracket is an opening bracket, we just add it to the front of the stack
   * 2. If the bracket is closed and the stack is Empty we need to stop as we found our broken bracket
   * 3. if we pop the stack and it's not the closing bracket for what we need we need to stop.
   * 4. We pop the stack and it's the closing bracket for what we need, we keep going - life is good
   */
  private def accumulate(accumulator: BracketAccumulator, bracket: Bracket): BracketAccumulator = {
    if (bracket.isOpeningBracket) {
      val updatedStack = accumulator.bracketStack.push(bracket)
      accumulator.copy(bracketStack = updatedStack)
    } else if (accumulator.bracketStack.isEmpty) {
        accumulator.copy(stoppedBracket = Some(bracket))
    } else {
      val popped = accumulator.bracketStack.pop()
      if (popped == bracket.findOppositeBracket) {
        accumulator.copy(bracketStack = accumulator.bracketStack)
      } else {
        accumulator.copy(stoppedBracket = Some(bracket))
      }
    }
  }

  /** Each bracket type has it's own score, we multiply it by the number of the type
   * we found then add them all */
  def calculateScore(inputBrackets: List[Brackets]): Int = {
    inputBrackets
      .map(Brackets.validateErrorCodes)
      .filter(validator => validator.stoppedBracket.isDefined)
      .flatMap(_.stoppedBracket)
      .map(lookupBracketScore)
      .sum
  }

  private def lookupBracketScore(bracket: Bracket): Int = bracket match {
    case RightRound => 3
    case RightSquare => 57
    case RightCurly => 1197
    case RightAngle => 25137
    case _ => 0
  }


  def parseInput(input: List[String]): List[Brackets] = {
    input.map(_.toCharArray.map(Brackets.bracketFromValue).toList)
      .map(Brackets.apply)
      .toList
  }
}