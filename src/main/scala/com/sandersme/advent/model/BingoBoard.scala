package com.sandersme.advent.model

case class Cell(number: Int, marked: Boolean)

class BingoBoard(val cellGrid: List[List[Cell]]) {

  def updateBoard(ballNumber: Int): BingoBoard = {
    val updatedCellGrid = cellGrid.map {row =>
      row.map(cell =>
        if (cell.number == ballNumber) cell.copy(marked = true) else cell
      )
    }

    BingoBoard(updatedCellGrid)
  }
  // Bingo is played on a 5x5 board
  // These boards have predetermined numbers and whether or not they are marked.

  def numberOfMarkedCells: Int = cellGrid.flatten.count(_.marked)

  // Winning if any rows are fully marked or any columns are fully marked
  // 1. see if all rows are marked
  // 2. see if all columns are marked
  //    To do that we need to get all the column heads and their index and
  //    Create a list of columns (e.g. we are rotating the matrix)
  //    Obviously swaping array values would be more efficient but we are just
  //    Coming up with brute force solutions.
  def hasBoardWon: Boolean = {
    val isAnyRowMarked = cellGrid
      .map(_.forall(_.marked))
      .count(allMarked => allMarked) > 0

    val isAnyColumnMarked = cellGrid.head
      .zipWithIndex
      .map{ case(columnHead, columnIndex) =>
        val rowRange = 1 until cellGrid.size
        columnHead :: rowRange.map{ row =>
          cellGrid(row)(columnIndex)
        }.toList
    }.map(_.forall(_.marked))
    .count(allMarked => allMarked) > 0

    isAnyRowMarked || isAnyColumnMarked
  }
  /**
   * start by finding the sum of all unmarked numbers on that board; in this case, the sum is 188.
   * Then, multiply that sum by the number that was just called when the board won, 24, to get
   * the final score, 188 * 24 = 4512.
   * // TODO Figure out how to track and pass the last ball called? This might be the subsystem that
   * // keeps track but we might want the board to track it
   * @return: Value of the final Score
   */
  def score(lastBallCalled: Int): Int = ???
}

object BingoBoard {
  def createBoard(input: List[List[Int]]): BingoBoard = {
    val grid: List[List[Cell]] = input.map{ row =>
      row.map(num => Cell(num, false)) // Set the cell to the number and marked to false
    }

    BingoBoard(grid)
  }
}