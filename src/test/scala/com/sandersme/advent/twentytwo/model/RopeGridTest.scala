package com.sandersme.advent.twentytwo.model

class RopeGridTest extends munit.FunSuite {
  val TEST_INPUT: List[String] = """R 4
                |U 4
                |L 3
                |D 1
                |R 4
                |D 1
                |L 5
                |R 2""".stripMargin
    .split("\n")
    .toList

  val TEST_ROPE_GRID: RopeGrid = RopeGrid.parseInput(TEST_INPUT)



  test("Validated that the parsed grid has defaults") {
    val expectedHead = Coord(0, 0)
    val expectedTail = List(Coord(0, 0))
    val expectedVisited = Set(Coord(0, 0))
    val expectedInstructions = RopeInstructions(List(
      RopeInstruction(Direction.R, 4),
      RopeInstruction(Direction.U, 4),
      RopeInstruction(Direction.L, 3),
      RopeInstruction(Direction.D, 1),
      RopeInstruction(Direction.R, 4),
      RopeInstruction(Direction.D, 1),
      RopeInstruction(Direction.L, 5),
      RopeInstruction(Direction.R, 2)
    ))

    assertEquals(TEST_ROPE_GRID.headCoord, expectedHead)
    assertEquals(TEST_ROPE_GRID.knotsCoord, expectedTail)
    assertEquals(TEST_ROPE_GRID.tailVisited, expectedVisited)
    assertEquals(TEST_ROPE_GRID.ropeInstructions, expectedInstructions)
  }

  test("Increment instructions should move the head, tail, tailvisited and increment instructions") {
    val updatedGrid = TEST_ROPE_GRID.incrementInstruction

    val expectedVisited = Set(
      Coord(0, 0),
      Coord(1, 0),
      Coord(2, 0),
      Coord(3, 0)
    )

    val originalSize = TEST_ROPE_GRID.ropeInstructions.instructions.size
    val updatedSize = updatedGrid.ropeInstructions.instructions.size

    assertEquals(updatedGrid.tailVisited, expectedVisited)
    assertEquals(originalSize - 1, updatedSize)
  }

  test("Increment all instructions. Final locations the tail visited should equal 13") {
    val updatedGrid = TEST_ROPE_GRID.incrementAllInstructions
    val numCoordsTailVisited = updatedGrid.numPlacesTailVisited
    val expectedCoordsVisited = 13

    assertEquals(numCoordsTailVisited, expectedCoordsVisited)
  }

  test("Test the longer grid with a tail of 9 knots") {
    val instructionInput = """R 5
                             |U 8
                             |L 8
                             |D 3
                             |R 17
                             |D 10
                             |L 25
                             |U 20""".stripMargin
      .split("\n").toList

    val ropeGrid = RopeGrid.parseInput(instructionInput, 9)
    val incrementAll = ropeGrid.incrementAllInstructions

    val numCoordsTailVisited = incrementAll.numPlacesTailVisited
    val expectedCoordsVisited = 36

    assertEquals(numCoordsTailVisited, expectedCoordsVisited)
  }
}
