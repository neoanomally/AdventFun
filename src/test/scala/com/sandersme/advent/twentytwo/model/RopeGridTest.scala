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

  test("Validate that the nine different positions around {X} should not require Tail to move") {
    val head: Coord = Coord(3, 3)

    val tailCoords = for {
      x <- 2 to 4
      y <- 2 to 4
    }  yield Coord(x, y)

    val ropeGrids = tailCoords.map{ tailCoord =>
      RopeGrid(head, tailCoord, Set(), RopeInstructions.empty)
    }

    val allWithinOne = ropeGrids.forall(_.isTailAdjacent)

    assertEquals(ropeGrids.size, 9)
    assertEquals(allWithinOne, true)
  }

  test("Validate that including things two steps away makes it so that there are 16 squares that aren't adjacent") {
    val head: Coord = Coord(3, 3)

    val tailCoords = for {
      x <- 1 to 5
      y <- 1 to 5
    }  yield Coord(x, y)

    val ropeGrids = tailCoords.map{ tailCoord =>
      RopeGrid(head, tailCoord, Set(), RopeInstructions.empty)
    }

    val numGridsNotAdjacent: Int = ropeGrids.count(!_.isTailAdjacent)
    val expectedNonAdjacentGrids = 16

    assertEquals(numGridsNotAdjacent, expectedNonAdjacentGrids)
  }

  test("Validated that the parsed grid has defaults") {
    val expectedHead = Coord(0, 0)
    val expectedTail = Coord(0, 0)
    val expectedVisited = Set(expectedTail)
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
    assertEquals(TEST_ROPE_GRID.tailCoord, expectedTail)
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
}
