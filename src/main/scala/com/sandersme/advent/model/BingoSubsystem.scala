package com.sandersme.advent.model

import com.sandersme.advent.model.BingoBoard

class BingoSubsystem(val boards: List[BingoBoard], drawingNumbers: List[Int]) {
  lazy val numberOfBoards: Int = boards.size
  lazy val numberOfDrawingBalls: Int = drawingNumbers.size

  def drawAndUpdateBoards: BingoSubsystem = {
    val currentDrawingNumber = drawingNumbers.head
    val updatedDrawingNumbers = drawingNumbers.tail

    val updatedBoards = boards.map(_.updateBoard(currentDrawingNumber))

    BingoSubsystem(updatedBoards, updatedDrawingNumbers)
  }

}

object BingoSubsystem {
  /**
   * The first line is always the random numbers that are used to call out for bingo cards.
   * Every empty line is to show the separation of boards.
   * Boards are then 5x5 collection of numbers separated by spaces.
   *
   * @param line: These are the inputs of line. This gets parsed in the logic above
   * @return a bingo subsystem which owns a collection of boards
   */
  def parseInput(input: List[String]): BingoSubsystem = {
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
