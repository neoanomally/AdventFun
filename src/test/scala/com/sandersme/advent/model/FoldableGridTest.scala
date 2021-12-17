package com.sandersme.advent.model

class FoldableGridTest extends munit.FunSuite {
  val TEST_INPUT = List("6,10","0,14","9,10","0,3","10,4","4,11","6,0","6,12",
    "4,1","0,13","10,12","3,4","3,0","8,4","1,10","2,14","8,10","9,0","","fold along y=7","fold along x=5")

  val TEST_FOLDABLE_GRID = FoldableGrid.parseInput(TEST_INPUT)

  test("Validate I can parse input and that the dimensions are based on max dimensions found") {
    val foldableGrid = FoldableGrid.parseInput(TEST_INPUT)

    assertEquals(foldableGrid.grid.size, 15)
    assertEquals(foldableGrid.grid.head.size, 11)

    assertEquals(foldableGrid.grid.head.head, false)
    assertEquals(foldableGrid.grid(4)(8), true)
    assertEquals(foldableGrid.grid(14)(2), true)
    assertEquals(foldableGrid.grid(12)(10), true)
    assertEquals(foldableGrid.grid(4)(7), false)
    assertEquals(foldableGrid.folds.size, 2)
    assertEquals(foldableGrid.folds.head, Point(0, 7))
  }

  test("Fold one step. Only implemented up") {
    val foldOneStep = FoldableGrid.foldOneStep(TEST_FOLDABLE_GRID)
    val foldTwoStep = FoldableGrid.foldOneStep(foldOneStep)
    assertEquals(foldOneStep.countVisibleDots, 17)
    assertEquals(foldTwoStep.countVisibleDots, 16)
  }

  test("Fold everything till folds are out") {
    val foldedCompletely = FoldableGrid.foldAllInstructions(TEST_FOLDABLE_GRID)
    assertEquals(foldedCompletely.countVisibleDots, 16)
  }

}
