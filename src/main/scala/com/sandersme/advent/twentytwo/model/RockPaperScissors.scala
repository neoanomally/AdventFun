package com.sandersme.advent.twentytwo.model

import com.sandersme.advent.twentytwo.model.HandShape

enum Outcome {
  case Lose, Draw, Win
}

object Outcome {
  def parseExpectedOutcome(input: String): Outcome = {
    input match {
      case "X" => Outcome.Lose
      case "Y" => Outcome.Draw
      case "Z" => Outcome.Win
      case _ => throw new Exception("Neither of the cases were expected. ")
    }
  }
}

case class ExpectedHandShapes(opponent: HandShape, suggested: HandShape) {
  def scoreFromOutcome(outcome: Outcome): Long = outcome match {
    case Outcome.Lose => 0L
    case Outcome.Draw => 3L
    case Outcome.Win  => 6L
  }

  def scoreFromRound: Long = {
    val handShapeScore = HandShape.shapeScore(suggested)
    val outcomeScore = scoreFromOutcome(expectedOutcome)

    outcomeScore + handShapeScore
  }

  def expectedOutcome: Outcome = {
    (opponent, suggested) match {
      case (o, s) if o == s => Outcome.Draw
      case (HandShape.Rock, HandShape.Paper) => Outcome.Win
      case (HandShape.Paper, HandShape.Scissors) => Outcome.Win
      case (HandShape.Scissors, HandShape.Rock) => Outcome.Win
      case _ => Outcome.Lose
    }
  }
}

object ExpectedHandShapes {
  def partOneParseFromInputLine(line: String): ExpectedHandShapes = {
    val split = line.split(" ")
    assert(split.length == 2)
    val opponent = HandShape.handShapeFromEncoder(split.head)
    val expected = HandShape.handShapeFromEncoder(split.last)

    ExpectedHandShapes(opponent, expected)
  }

  /**
   * TODO: Run a Test
   * The recommended hand that is based on both the opponents expected hand and
   * the recommended Outcome.
   */
  def recommendedHand(opponentHand: HandShape, recommendedOutcome: Outcome): HandShape = {
    (opponentHand, recommendedOutcome) match {
      case (_, Outcome.Draw) => opponentHand
      case (HandShape.Rock, Outcome.Win) => HandShape.Paper
      case (HandShape.Paper, Outcome.Win) => HandShape.Scissors
      case (HandShape.Scissors, Outcome.Win) => HandShape.Rock
      case (HandShape.Rock, Outcome.Lose) => HandShape.Scissors
      case (HandShape.Paper, Outcome.Lose) => HandShape.Rock
      case (HandShape.Scissors, Outcome.Lose) => HandShape.Paper
    }
  }

  /**
   * TODO: Create a test for this method.
   * @param line
   * @return
   */
  def partTwoParseFromInputLine(line: String): ExpectedHandShapes = {
    val split = line.split(" ")
    assert(split.length == 2)
    val opponent = HandShape.handShapeFromEncoder(split.head)
    val outcome = Outcome.parseExpectedOutcome(split.last)
    val recommendedHand = ExpectedHandShapes.recommendedHand(opponent, outcome)

    ExpectedHandShapes(opponent, recommendedHand)
  }
}

object RockPaperScissors {

  def partOneScoresFromInput(input: Seq[String]): Seq[Long] = {
    input
      .map(ExpectedHandShapes.partOneParseFromInputLine)
      .map(_.scoreFromRound)
  }

  def partOneTotalScoreFromInput(input: Seq[String]): Long = {
    partOneScoresFromInput(input)
      .sum
  }

  def partTwoTotalScoreFromInput(input: Seq[String]): Long = {
    input
      .map(ExpectedHandShapes.partTwoParseFromInputLine)
      .map(_.scoreFromRound)
      .sum
  }
}
