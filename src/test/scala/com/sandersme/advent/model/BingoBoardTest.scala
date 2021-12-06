package com.sandersme.advent.model

class BingoBoardTest extends munit.FunSuite {
  val TEST_INPUT: List[List[Int]] = List(
    List(22, 13, 17, 11,  0),
    List(8,  2, 23,  4, 24),
    List(21,  9, 14, 16,  7),
    List(6, 10,  3, 18,  5),
    List(1, 12, 20, 15, 19)
  )

  val board: BingoBoard = BingoBoard.createBoard(TEST_INPUT)

  test("Validate BingoBoard update updates a marked cell") {
    val newBoardMarkedCellSize: Int = board.numberOfMarkedCells
    val expectedEmptyCellBoardSize = 0

    val updatedBoard = board.updateBoard(22)
    val updatedBoardMarkedCellSize: Int = updatedBoard.numberOfMarkedCells
    val expectedUpdatedBoardCellSize = 1

    assertEquals(newBoardMarkedCellSize, expectedEmptyCellBoardSize)
    assertEquals(updatedBoardMarkedCellSize, expectedUpdatedBoardCellSize)
  }

  test("Check if a board is a winner after adding 5 values to the second row") {
    // values we need to add: 8,  2, 23,  4, 24

    val initalBoardShouldFail = board.hasBoardWon
    val expectedFailed = false
    val expectedWon = true

    val updatedFourTimesShouldFail = board.updateBoard(8)
      .updateBoard(2).updateBoard(23).updateBoard(4)

    val finalBoardAfterFourUpdates = updatedFourTimesShouldFail.updateBoard(24)

    assertEquals(updatedFourTimesShouldFail.hasBoardWon, expectedFailed)
    assertEquals(initalBoardShouldFail, expectedFailed)
    assertEquals(finalBoardAfterFourUpdates.hasBoardWon, expectedWon)
  }


  test("Check if a board is a winner after adding 5 values to the third column") {
    val valuesToAdd = List(17, 23, 14, 3, 20)

    val initalBoardShouldFail = board.hasBoardWon
    val expectedFailed = false
    val expectedWon = true

    val finalBoard = valuesToAdd.foldLeft(board)((board, nextValue) => {
      board.updateBoard(nextValue)
    })

    assertEquals(initalBoardShouldFail, expectedFailed)
    assertEquals(finalBoard.hasBoardWon, expectedWon)
  }

  test("Score the board based on the last ball called") {
    assert(false)
  }
}
