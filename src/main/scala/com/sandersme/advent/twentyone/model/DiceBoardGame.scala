package com.sandersme.advent.twentyone.model

import scala.annotation.tailrec
import scala.collection.mutable

case class DiceBoardGame(players: List[DicePlayer], dice: DeterministicDice) {

  def numTimesDiceRolled: Int = dice.timesRolled

  def lowestPlayerScore: Int = players.minBy(_.score).score

  def calculateAdventScore: Long = lowestPlayerScore * numTimesDiceRolled

  def winningPlayer: DicePlayer = players.maxBy(_.score)

  /**
   * Takes the current player (e.g. the player at the front of the line) and rolls
   * the dice three times.
   *
   * The player takes the results to move forward and updates its space and the score.
   * @return
   */
  def nextPlayerMoveForward: DiceBoardGame = {
    val currentPlayer = players.head
    val nextPlayers = players.tail

    val (updatedDice, diceResults) = dice.rollThreeTimes
    val updatedPlayer = currentPlayer.moveFoward(diceResults)

    DiceBoardGame(nextPlayers :+ updatedPlayer, updatedDice)
  }

}

object DiceBoardGame {
  val WINNING_SCORE = 1000

  def parseInput(input: List[String]): DiceBoardGame = {
    val player1Location: Int = input.head.takeRight(1).toInt
    val player2Location: Int = input.last.takeRight(1).toInt

    val player1 = DicePlayer(1, player1Location)
    val player2 = DicePlayer(2, player2Location)
    val dice = DeterministicDice.defaultDice

    DiceBoardGame(List(player1, player2), dice)
  }

  /**
   * This method will continue to go through the set of players until we have a winner
   * The Diceboard knows how to pick which player moves forward and use the deterministic dice.
   * @param diceBoardGame
   * @return
   */
  @tailrec
  def rollDiceUntilWinner(diceBoardGame: DiceBoardGame): DiceBoardGame = {
    if( hasAWinner(diceBoardGame)) {
      diceBoardGame
    } else {
      rollDiceUntilWinner(diceBoardGame.nextPlayerMoveForward)
    }
  }

  /**
   * Checks to see if a board has a winning score.
   * @param diceBoardGame
   * @return
   */
  def hasAWinner(diceBoardGame: DiceBoardGame): Boolean = {
    diceBoardGame.players.exists(_.score >= WINNING_SCORE)
  }

  case class Lookup(score: Int, position: Int, diceResult: Int)
  case class Results(steps: Int)

  type LookupMap = Map[Lookup, Results]
  type DiceResult = Int
  type Frequency = Int

  def tripleDiceCombinations: Map[DiceResult, Frequency] = {
    val results = for {
      resultThrowOne <- 1 to 3
      resultThrowTwo <- 1 to 3
      resultThrowThree <- 1 to 3
    } yield resultThrowOne + resultThrowTwo + resultThrowThree

    results.groupBy(identity)
      .map{case (k, values) => k -> values.size}

  }

  case class GamePositionCombinations(startingPos: Int, endingPos: Int, frequency: Int)

  def gamePositionDiceCombinations: List[GamePositionCombinations] = {
    val diceCombinations = tripleDiceCombinations
    val results = for {
      position   <- 1 to 10
      diceResult <- diceCombinations

      updatedPosition = ((position - 1 + diceResult._1) % 10) + 1
    } yield GamePositionCombinations(position, updatedPosition, diceResult._2)

    results.toList
  }

  case class PositionScore(player1Pos: Int, player1Score: Int, player2Pos: Int, player2Score: Int)

  object PositionScore {
    def fromInput(input: List[String]): PositionScore = {
      val p1Pos = input.head.takeRight(1).toInt
      val p2Pos = input.last.takeRight(1).toInt

      PositionScore(p1Pos, 0, p2Pos, 0)
    }
  }

  def generateDiceCombos: Seq[Int] = for {
    resultThrowOne <- 1 to 3
    resultThrowTwo <- 1 to 3
    resultThrowThree <- 1 to 3
  } yield resultThrowOne + resultThrowTwo + resultThrowThree


  // TODO figure out how to use the frequencies to make this much faster
  // TODO: I need to simulate each round. Instead of just having one player go, we have both players increment.
  // The position Score is going to now keep track of player1 and player two
  def calculateNumUniversesWon(positionScore: PositionScore,
                               frequency: BigInt,
                               cache: mutable.Map[PositionScore, (BigInt, BigInt)] = new mutable.HashMap(),
                               diceCombos: Seq[GamePositionCombinations] = gamePositionDiceCombinations): (BigInt, BigInt) = {
    if (cache.contains(positionScore)) {
      println("CACHED")
      cache(positionScore)
    } else if (positionScore.player1Score >= 21) {
      (frequency, BigInt(0))
    } else if (positionScore.player2Score >= 21) {
      (BigInt(0), frequency)
    } else {
      val positionScoreCombinations = for {
        player1Dice <- diceCombos.filter(_.startingPos == positionScore.player1Pos)
        player2Dice <- diceCombos.filter(_.startingPos == positionScore.player2Pos)

        p1Pos = player1Dice.endingPos
        p2Pos = player2Dice.endingPos
        p1Score = positionScore.player1Score + p1Pos
        p2Score = positionScore.player2Score + p2Pos

        updatedPositionScore = PositionScore(p1Pos, p1Score, p2Pos, p2Score)
        updatedFrequency = player1Dice.frequency * player2Dice.frequency * frequency

        results = calculateNumUniversesWon(updatedPositionScore, updatedFrequency, cache, diceCombos)
      } yield results

      val summedResults: (BigInt, BigInt) = positionScoreCombinations
        .reduce((left, right) => (left._1 + right._1, left._2 + right._2))
      cache.update(positionScore, summedResults)

      summedResults
    }
  }

  def quantumDiceGame(player1Starting: Int, player2Starting: Int): (BigInt, BigInt) = {
    val starting = PositionScore(player1Starting, 0, player2Starting, 0)
    quantumDiceGame(starting)
  }

  def quantumDiceGame(positionScore: PositionScore): (BigInt, BigInt) = {
    val winners = bruteForceUniversesWon(positionScore)

    (winners._1 / 27, winners._2)
  }


  // TODO figure out recursion first
  // TODO I figured out why p1 wins by a factor of 27. This happens because when Player 1 wins. The previous
  // roll spawns 27 universes for the combinations that player 2 may win. I'm fine keeping it with how it
  // is knowing that it's up by a factor of 27. What I could do is incrementally build up the turns, but
  // that includes additional logic .
  // TODO: I need to simulate each round. Instead of just having one player go, we have both players increment.
  // The position Score is going to now keep track of player1 and player two
  def bruteForceUniversesWon(positionScore: PositionScore,
                             cache: mutable.Map[PositionScore, (BigInt, BigInt)] = new mutable.HashMap(),
                             diceCombos: Seq[Int] = generateDiceCombos): (BigInt, BigInt) = {
    if (positionScore.player1Score >= 21) {
      (BigInt(1), BigInt(0))
    } else if (positionScore.player2Score >= 21) {
      (BigInt(0), BigInt(1))
    } else if (cache.contains(positionScore)) {
      cache(positionScore)
    }else{
      val allResults = for {
        player1Dice <- diceCombos
        player2Dice <- diceCombos

        p1Pos = ((positionScore.player1Pos - 1 + player1Dice) % 10) + 1
        p2Pos = ((positionScore.player2Pos - 1 + player2Dice) % 10) + 1
        p1Score = positionScore.player1Score + p1Pos
        p2Score = positionScore.player2Score + p2Pos

        updatedPositionScore = PositionScore(p1Pos, p1Score, p2Pos, p2Score)
      } yield bruteForceUniversesWon(updatedPositionScore, cache, diceCombos)

      val summedResults = (allResults.map(_._1).sum, allResults.map(_._2).sum)

      cache.update(positionScore, summedResults)
      summedResults
    }
  }

}
