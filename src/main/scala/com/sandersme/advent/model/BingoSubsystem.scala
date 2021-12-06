package com.sandersme.advent.model

import com.sandersme.advent.model.BingoBoard

import scala.annotation.tailrec

/**
 * The bingo subsystem is used to manage boards and the random numbers that
 * will be drawn. It also keeps track of the currentBall that has been drawn.
 * I was torn between using a -1 or an Option, for the first state.
 * Realistically having a -1 should have no negative impacts. This ball may
 * be useless. We may want to make the currentBall
 *
 * @param boards
 * @param drawingNumbers
 * @param currentBall
 */
case class BingoSubsystem(boards: List[BingoBoard], drawingNumbers: List[Int],
                          calledDrawingNumbers: List[Int] = List.empty) {
  import com.sandersme.advent.model.BingoSubsystem._

  lazy val numberOfBoards: Int = boards.size
  lazy val numberOfDrawingBalls: Int = drawingNumbers.size

  def drawAndUpdateBoards: BingoSubsystem = {
    val currentDrawingNumber = drawingNumbers.head
    val updatedDrawingNumbers = drawingNumbers.tail

    val updatedBoards = boards.map(_.updateBoard(currentDrawingNumber))
    val updatedBallsCalled = calledDrawingNumbers :+ currentDrawingNumber
    BingoSubsystem(updatedBoards, updatedDrawingNumbers, updatedBallsCalled)
  }

  /**
   * This should be pretty easy.
   * 1. Find all the boards that have won
   * 2. Go through all the numbers in drawn order and find all the boards with that winning
   *    number.
   * // TODO there is probably a tailRecusiveWay to do this that is more efficient, but that
   * // would be a future optimization. The current time complexity is calledDrawingNumber * boardsWon
   * // It would be more efficient to remove boards as winners are found OR to loop through the boards
   * // and look up the index value for each Drawn number.
   * @return
   */
  def findOrderedWinningBoards: List[BingoBoard] = {
    val winningBoards = boards
      .filter(_.hasBoardWon)

    val boardsByWinningNumber: List[BingoBoard] = calledDrawingNumbers
      .flatMap(ball => winningBoards.filter(_.winningNumber.get == ball))

    boardsByWinningNumber
  }

  def findTheLastWonBoard: BingoBoard = findOrderedWinningBoards.last

  def findTheFirstWonBoard: BingoBoard = findOrderedWinningBoards.head

  def drawAllBalls: BingoSubsystem = drawAllBallsRecursion(this)

  def drawUntilWinner: BingoSubsystem = drawUpdateUntilWinner(this)

  def anyBoardHasWinner: Boolean = boards.exists(_.hasBoardWon)

  def winningBoard: Option[BingoBoard] = boards.find(_.hasBoardWon)

  def hasDrawingNumbersLeft: Boolean = drawingNumbers.nonEmpty

  def winningBoardScore: Option[Int] = boards
    .find(_.hasBoardWon)
    .map(_.score)

  def currentBall: Int = calledDrawingNumbers.last
}

object BingoSubsystem {
  @tailrec
  private def drawAllBallsRecursion(subsystem:BingoSubsystem): BingoSubsystem = {
    if (subsystem.hasDrawingNumbersLeft) {
      val updatedSubsystem = subsystem.drawAndUpdateBoards
      drawAllBallsRecursion(updatedSubsystem)
    } else {
      subsystem
    }

  }

  @tailrec
  private def drawUpdateUntilWinner(subsytem: BingoSubsystem): BingoSubsystem = {
    val hasNoBallsLeft = subsytem.numberOfDrawingBalls == 0
    if (subsytem.anyBoardHasWinner || hasNoBallsLeft) {
      subsytem
    } else {
      drawUpdateUntilWinner(subsytem.drawAndUpdateBoards)
    }
  }

  /**
   * The first line is always the random numbers that are used to call out for bingo cards.
   * Every empty line is to show the separation of boards.
   * Boards are then 5x5 collection of numbers separated by spaces.
   *
   * @param line: These are the inputs of line. This gets parsed in the logic above
   * @return a bingo subsystem which owns a collection of boards
   */
  def fromInput(input: List[String]): BingoSubsystem = {
    val drawingBalls: List[Int] = input
      .head // This will fail on bad input
      .split(',')
      .map(_.trim)
      .map(_.toInt)
      .toList

    val boards: List[BingoBoard] = input
      .tail // ignore the head
      .filter(_.nonEmpty) // Remove the empty lines
      .map(_.split(' ').filter(_.trim.nonEmpty).map(_.toInt).toList) // Split lines up spaces convert to Numbers
      .grouped(5) // Separte these into groups of five.
      .map(f => BingoBoard.createBoard(f))
      .toList

    BingoSubsystem(boards, drawingBalls)
  }
}
