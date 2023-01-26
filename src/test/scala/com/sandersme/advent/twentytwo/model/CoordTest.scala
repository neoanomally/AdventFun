package com.sandersme.advent.twentytwo.model

class CoordTest extends munit.FunSuite {
  test("Test to check if the headCoord is adjacent to the tailCoord") {
    val headCoord = Coord(3, 3)
    val tailCoord = Coord(2, 2)

    val isAdjacent = Coord.isTailAdjacent(headCoord, tailCoord)

    assertEquals(isAdjacent, true)
  }

  test("Test to check if the headCoord is not adjacent to the tailCoord") {
    val headCoord = Coord(3, 3)
    val tailCoord = Coord(1, 2)

    val isAdjacent = Coord.isTailAdjacent(headCoord, tailCoord)

    assertEquals(isAdjacent, false)
  }

  test("Move the tail coord until it's adjacent to the head coord when it doesn't need to move should be identity") {
    val headCoord = Coord(3, 3)
    val tailCoord = Coord(2, 2)

    val movedCoords: Coord = Coord.moveTailUntilAdjacent(headCoord, tailCoord)

    val expectedCoords = tailCoord
    assertEquals(movedCoords, expectedCoords)
  }

  test("Move the tail coord until it's adjacent to the head coord when it doesn't need to move should have two") {
    val headCoord = Coord(3, 3)
    val tailCoord = Coord(1, 2)

    val movedCoords: Coord = Coord.moveTailUntilAdjacent(headCoord, tailCoord)

    val expectedCoords = Coord(2, 2)
    assertEquals(movedCoords, expectedCoords)
  }

  test("Move the tail coord that's much further away from the head that takes multiple iterations") {
    val headCoord = Coord(3, 3)
    val tailCoord = Coord(-4, -4)

    val updatedTailCoord: Coord = Coord.moveTailUntilAdjacent(headCoord, tailCoord)

    val expectedTailCoord = Coord(2, 2)

    assertEquals(updatedTailCoord, expectedTailCoord)
  }


  test("Move the tail coord that's much further away from the head that is to the NE") {
    val headCoord = Coord(3, 3)
    val tailCoord = Coord(9, 9)

    val updatedTailCoord: Coord = Coord.moveTailUntilAdjacent(headCoord, tailCoord)

    val expectedTailCoord = Coord(4, 4)

    assertEquals(updatedTailCoord, expectedTailCoord)
  }

  test("Validate two coords are neither in the same column or row") {
    val headCoord = Coord(3, 3)
    val tailCoord = Coord(9, 9)

    val isNotInRowOrColumn = Coord.isNotInRowOrColumn(headCoord, tailCoord)

    assertEquals(isNotInRowOrColumn, true)
  }

  test("Validate two coords are in the same column") {
    val headCoord = Coord(3, 3)
    val tailCoordA = Coord(9, 3)
    val tailCoordB = Coord(3, 9)

    val isInColumn = Coord.isNotInRowOrColumn(headCoord, tailCoordA)
    val isInRow = Coord.isNotInRowOrColumn(headCoord, tailCoordA)

    assertEquals(isInRow, false)
    assertEquals(isInRow, false)
  }


  test("Increment tail that should be a diagonal") {
    val headCoord = Coord(3, 3)
    val tailCoordA = List(Coord(9, 9))
    val tailCoordB = List(Coord(1, 1))

    val incrementedTailA = Coord.incrementTail(headCoord, tailCoordA)
    val incrementedTailB = Coord.incrementTail(headCoord, tailCoordB)

    val expectedTailA = List(Coord(8, 8))
    val expectedTailB = List(Coord(2, 2))

    assertEquals(incrementedTailA, expectedTailA)
    assertEquals(incrementedTailB, expectedTailB)
  }

  test("Increment tail when it should be an X increment") {
    val headCoord = Coord(3, 3)
    val tailCoord = List(Coord(5, 3))

    val incrementTail = Coord.incrementTail(headCoord, tailCoord)
    val expectedTail = List(Coord(4, 3))

    assertEquals(incrementTail, expectedTail)
  }

  test("check if the Coord(-1,-2) Tail Coord: Coord(0,0) moving Y Direction") {
    val headCoord = Coord(-1, -2)
    val tailCoord = Coord(0, 0)

    val isNotInRowOrColumn = Coord.isNotInRowOrColumn(headCoord, tailCoord)
    assertEquals(isNotInRowOrColumn, true)
  }
}
