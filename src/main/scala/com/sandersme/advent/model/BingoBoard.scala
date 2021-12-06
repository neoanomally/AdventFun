package com.sandersme.advent.model

case class Cell(number: Int, marked: Boolean)

case class BingoBoard(cellGrid: List[List[Cell]], winningNumber: Option[Int] = None) {

  def updateBoard(ballNumber: Int): BingoBoard = {
    if  (winningNumber.isDefined) {
      this
    } else {
      val updatedCellGrid = cellGrid.map {row =>
        row.map(cell =>
          if (cell.number == ballNumber) cell.copy(marked = true) else cell
        )
      }

      val board = copy(cellGrid = updatedCellGrid)

      if (board.winningNumber.isEmpty && board.hasBoardWon) {
        board.copy(winningNumber = Some(ballNumber))
      } else {
        board
      }
    }
  }

  // Bingo is played on a 5x5 board
  // These boards have predetermined numbers and whether or not they are marked.
  def numberOfMarkedCells: Int = cellGrid.flatten.count(_.marked)

  def hasBoardWon: Boolean = winningNumber.isDefined || checkBoardWon

  // Winning if any rows are fully marked or any columns are fully marked
  // 1. see if all rows are marked
  // 2. see if all columns are marked
  // Note: We are using List.exists in the auxillary functions. This should early stop
  private def checkBoardWon: Boolean = {
    isAnyRowFullyMarked || isAnyColumnFullyMarked
  }

  def boardHasNotWon: Boolean = !checkBoardWon

  /**
   * Loop through each row and find if a Row has all the values marked.
   * @return true if any row has all cells marked left to right
   */
  private def isAnyRowFullyMarked: Boolean = {
    cellGrid
      .exists(_.forall(_.marked))
  }

  /**
   * 1. Grab the top most row from the the cell
   * 2. Loop through the column of the first Row with the column index
   * 3. Use exists because it does early stopping on the first time column has all cells marked
   *
   * @return Whether or not a column in a 5x5 grid has marked all true
   */
  private def isAnyColumnFullyMarked: Boolean = {
    cellGrid.head
      .zipWithIndex
      .exists{ case(_, columnIndex) =>
        cellGrid.indices.map{ row =>
          cellGrid(row)(columnIndex)
        }.forall(_.marked)
      }
  }

  /**
   * start by finding the sum of all unmarked numbers on that board; in this case, the sum is 188.
   * Then, multiply that sum by the number that was just called when the board won, 24, to get
   * the final score, 188 * 24 = 4512.
   * // TODO Figure out how to track and pass the last ball called? This might be the subsystem that
   * // keeps track but we might want the board to track it
   * @return: Value of the final Score
   */
  def score: Int = {
    val sumOfNotMarkedCells = cellGrid
      .flatten
      .filterNot(_.marked)
      .map(_.number)
      .sum

      sumOfNotMarkedCells * winningNumber.getOrElse(0)
  }
}

object BingoBoard {
  def createBoard(input: List[List[Int]]): BingoBoard = {
    val grid: List[List[Cell]] = input.map{ row =>
      row.map(num => Cell(num, false)) // Set the cell to the number and marked to false
    }

    BingoBoard(grid)
  }
}