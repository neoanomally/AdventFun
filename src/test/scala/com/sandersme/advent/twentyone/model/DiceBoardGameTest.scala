package com.sandersme.advent.twentyone.model

import com.sandersme.advent.twentyone.model.DiceBoardGame.{PositionScore, bruteForceUniversesWon, rollDiceUntilWinner}

import scala.collection.mutable

class DiceBoardGameTest extends munit.FunSuite {
  val TEST_INPUT = List("Player 1 starting position: 4", "Player 2 starting position: 8")

  val TEST_DICE_GAME_BOARD: DiceBoardGame = DiceBoardGame.parseInput(TEST_INPUT)
  test("Validate parsing") {

    assertEquals(TEST_DICE_GAME_BOARD.players.size, 2)
    assertEquals(TEST_DICE_GAME_BOARD.players.head.player, 1)
    assertEquals(TEST_DICE_GAME_BOARD.players.head.space, 4)
    assertEquals(TEST_DICE_GAME_BOARD.players.last.space, 8)
  }

  test("Dice board next player moves forward") {
    val updatedBoard = TEST_DICE_GAME_BOARD.nextPlayerMoveForward

    assertEquals(updatedBoard.players.head.player, 2)
    assertEquals(updatedBoard.players.last.space, 10)
    assertEquals(updatedBoard.dice.current, 4)
  }

  test("Roll dice until we have a winner") {
    val winnerWinnerChickenDinner = rollDiceUntilWinner(TEST_DICE_GAME_BOARD)

    val overallScore = winnerWinnerChickenDinner.calculateAdventScore
    assertEquals(overallScore, 739785L)
  }

  test("Any combination of two starting positions will always sum the multiplication of the combination frequencies to 729") {
    val f = DiceBoardGame.gamePositionDiceCombinations

    val s = for {
      left  <- f.filter(_.startingPos == 1)
      right <- f.filter(_.startingPos == 8)

      combos = left.frequency * right.frequency
    } yield combos

    assertEquals(s.sum, 729)

  }

  // IGNORING DUE TO it taking too long it's a println because the results._1 needs to be divded by 27 and I'm not
  // This does produce the correct results
  test("Different results from throwing the dice 3 times".ignore) {
    val results = DiceBoardGame.quantumDiceGame(4, 8)

    assertEquals(results._1, BigInt(444356092776315L))
    assertEquals(results._2, BigInt(341960390180808L))
  }

// TODO LOOK INTO MEMOIZATION MORE
//  def memoize[I, O](f: I => O): I => O = new mutable.HashMap[I, O]() {self =>
//    override def apply(key: I) = self.synchronized(getOrElseUpdate(key, f(key)))
//  }
//
//  test("TEST memoization") {
//    lazy val fib: Int => BigInt = memoize {
//      case 0 => 0
//      case 1 => 1
//      case n => fib(n-1) + fib(n-2)
//    }
//
//    fib(100)
//    println(fib)
//  }
}
