package com.sandersme.advent.twentyone

import com.sandersme.advent.twentyone.model.BingoSubsystem
import com.sandersme.advent.twentyone.Input


object GiantSquidBingo {

  /**
   *
   * You're already almost 1.5km (almost a mile) below the surface of the ocean, already so deep that you can't
   * see any sunlight. What you can see, however, is a giant squid that has attached itself to the outside of
   * your submarine.
   * Maybe it wants to play bingo?
   * Bingo is played on a set of boards each consisting of a 5x5 grid of numbers. Numbers are chosen at random,
   * and the chosen number is marked on all boards on which it appears. (Numbers may not appear on all boards.)
   * If all numbers in any row or any column of a board are marked, that board wins. (Diagonals don't count.)
   * The submarine has a bingo subsystem to help passengers (currently, you and the giant squid) pass the time.
   * It automatically generates a random order in which to draw numbers and a random set of boards (your puzzle input).
   * SEE TESTS FOR THE EXAMPLE BOARDS
   *
   * At this point, the third board wins because it has at least one complete row or column of marked numbers
   * (in this case, the entire top row is marked: 14 21 17 24 4).
   * The score of the winning board can now be calculated. Start by finding the sum of all unmarked numbers on that
   * board; in this case, the sum is 188. Then, multiply that sum by the number that was just called when the board
   * won, 24, to get the final score, 188 * 24 = 4512.
   * To guarantee victory against the giant squid, figure out which board will win first. What will your final
   * score be if you choose that board?
   * @param args
   */
  def main(args: Array[String]): Unit = {
    val inputData = Input.readFromDataResource("day4_input")

    val bingoSubsystem =  BingoSubsystem
      .fromInput(inputData)

    val winningScoreAfterWinner = bingoSubsystem
      .drawUntilWinner
      .winningBoardScore

    val drawAllFindLastWonScore = bingoSubsystem
      .drawAllBalls
      .findTheLastWonBoard.score

    println(s"First board with winning score $winningScoreAfterWinner")
    println(s"The last won board score will be $drawAllFindLastWonScore")
  }
}
