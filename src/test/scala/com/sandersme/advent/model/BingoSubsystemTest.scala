package com.sandersme.advent.model

import com.sandersme.advent.model.BingoSubsystem.drawUpdateUntilWinner

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

  val stubBingoSubsystem: BingoSubsystem = BingoSubsystem.fromInput(TEST_INPUT)

    test("Validate can parse Input") {
      val expectedNumberOfBoards = 3
      val expectedNumberOfDrawingBalls = 27

      assertEquals(stubBingoSubsystem.numberOfDrawingBalls, expectedNumberOfDrawingBalls)
      assertEquals(stubBingoSubsystem.numberOfBoards, expectedNumberOfBoards)
    }

  test("Update all boards with some number") {
    // The number 7 is in three boards so we'll test that.
    val expectedNumberOfBallsLeft = 26 // Make sure we are decrementing
    val expectedNumberOfNewBoardsMarked = 0
    val expectedNumberOfUpdatedBoardsMarked = 3
    val newNumberOfBoardsMarked = stubBingoSubsystem.boards.map(_.numberOfMarkedCells).sum

    val updatedBoardSubsystem = stubBingoSubsystem.drawAndUpdateBoards // Should be 7
    val updatedNumberOfBoardsMarked = updatedBoardSubsystem.boards.map(_.numberOfMarkedCells).sum
    val updatedNumberOfBallsLeft = updatedBoardSubsystem.numberOfDrawingBalls

    assertEquals(newNumberOfBoardsMarked, expectedNumberOfNewBoardsMarked)
    assertEquals(updatedNumberOfBoardsMarked, expectedNumberOfUpdatedBoardsMarked)
    assertEquals(updatedNumberOfBallsLeft, expectedNumberOfBallsLeft)
  }

  test("Update all the boards and have no winner") {
    val updatedOnceShouldHaveNoWinner = stubBingoSubsystem
      .drawAndUpdateBoards.anyBoardHasWinner

    val expectedNoWinner = false

    assertEquals(updatedOnceShouldHaveNoWinner, expectedNoWinner)
  }

  test("Calculate the score of the winning board based on test input cases") {
    val bingoSubsystemAtWinner = stubBingoSubsystem.drawUntilWinner
    val expectedWinningScore = 4512

    val winningScore = bingoSubsystemAtWinner.winningBoardScore.getOrElse(0)

    assertEquals(winningScore, expectedWinningScore)
  }

  test("Draw board has a winner and check the winning ball") {
    val startingBingoSubsytemIsNotAWinner = stubBingoSubsystem.anyBoardHasWinner
    val expectedNoWinner = false
    val expectedWinner = true

    val bingoSubsystemAtWinner = stubBingoSubsystem.drawUntilWinner
    val winningBallNumber = bingoSubsystemAtWinner.currentBall
    val expectedWinningBall = 24

    assertEquals(startingBingoSubsytemIsNotAWinner, expectedNoWinner)
    assertEquals(winningBallNumber, expectedWinningBall)
  }

  test("Draw balls until there are none left and that the drawn balls are there") {
    val numberOfDrawingBallsStart = stubBingoSubsystem.numberOfDrawingBalls
    val drawnSubsystem = stubBingoSubsystem.drawAllBalls
    val numberOfDrawingBallsEnd = drawnSubsystem.numberOfDrawingBalls

    val expectedStartingNumberOfBalls = 27
    val expectedNumberOfBallsAfterDrawingEnd = 0

    val numberOfBallsCalled = drawnSubsystem.calledDrawingNumbers.size

    assertEquals(numberOfDrawingBallsStart, 27)
    assertEquals(numberOfDrawingBallsEnd, expectedNumberOfBallsAfterDrawingEnd)
    assertEquals(numberOfBallsCalled, numberOfDrawingBallsStart)
  }


  test("Validate the order that balls were drawn are the same as the previous balls called after drawn") {
    val expectedOutput = stubBingoSubsystem.drawingNumbers
    val results = stubBingoSubsystem.drawAllBalls.calledDrawingNumbers

    assertEquals(expectedOutput, results)
  }

  test("Find the last won board should equal 1924") {
    val lastWonBoard = stubBingoSubsystem
      .drawAllBalls
      .findTheLastWonBoard

    val expectedFinalScoreOfBoard = 1924

    assertEquals(lastWonBoard.score, expectedFinalScoreOfBoard)
  }
}
