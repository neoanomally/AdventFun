package com.sandersme.advent.model

class BingoSubsystemTest extends munit.FunSuite {
  val TEST_INPUT: List[String] = List(
    "7,4,9,5,11,17,23,2,0,14,21,24,10,16,13,6,15,25,12,22,18,20,8,19,3,26,1", "",
    "22 13 17 11  0",
    "8  2 23  4 24",
    "21  9 14 16  7",
    "6 10  3 18  5",
    "1 12 20 15 19", "",
    "3 15  0  2 22",
    "9 18 13 17  5",
    "19  8  7 25 23",
    "20 11 10 24  4",
    "14 21 16 12  6", "",
    "14 21 17 24  4",
    "10 16 15  9 19",
    "18  8 23 26 20",
    "22 11 13  6  5",
    "2  0 12  3  7"
  )

  val bingoSubsystem: BingoSubsystem = BingoSubsystem.parseInput(TEST_INPUT)

    test("Validate can parse Input") {
      val expectedNumberOfBoards = 3
      val expectedNumberOfDrawingBalls = 27

      assertEquals(bingoSubsystem.numberOfDrawingBalls, expectedNumberOfDrawingBalls)
      assertEquals(bingoSubsystem.numberOfBoards, expectedNumberOfBoards)
    }

  test("Update all boards with some number") {
    // The number 7 is in three boards so we'll test that.
    val expectedNumberOfBallsLeft = 26 // Make sure we are decrementing
    val expectedNumberOfNewBoardsMarked = 0
    val expectedNumberOfUpdatedBoardsMarked = 3
    val newNumberOfBoardsMarked = bingoSubsystem.boards.map(_.numberOfMarkedCells).sum

    val updatedBoardSubsystem = bingoSubsystem.drawAndUpdateBoards // Should be 7
    val updatedNumberOfBoardsMarked = updatedBoardSubsystem.boards.map(_.numberOfMarkedCells).sum
    val updatedNumberOfBallsLeft = updatedBoardSubsystem.numberOfDrawingBalls

    assertEquals(newNumberOfBoardsMarked, expectedNumberOfNewBoardsMarked)
    assertEquals(updatedNumberOfBoardsMarked, expectedNumberOfUpdatedBoardsMarked)
    assertEquals(updatedNumberOfBallsLeft, expectedNumberOfBallsLeft)
  }

  test("Keep updating boards until we find a winner and grab that board") {
    assert(false)
  }

  test("Score board with the ball that was used to score last") {

  }
}
