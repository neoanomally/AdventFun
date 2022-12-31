package com.sandersme.advent.twentytwo.model

import munit.FunSuite

class TreeGridTest extends munit.FunSuite {

  val TEST_GRID_INPUT: List[String] =
    """30373
      |25512
      |65332
      |33549
      |35390""".stripMargin
    .split("\n")
    .toList

  val TEST_TREE_GRID: TreeGrid = TreeGrid.parseInput(TEST_GRID_INPUT)

    test("Test that we can parse the tree grid input into a tree grid") {
      assertEquals(TEST_TREE_GRID.treeAt(2, 2), Tree(3))
    }

    test("Validate that the height and width of the tree is 5x5") {
      assertEquals(TEST_TREE_GRID.gridHeight, 4)
      assertEquals(TEST_TREE_GRID.gridWidth, 4)
    }

    test("Validate if Tree is visible. All edges should be visible" ) {
      val topAtEdge = TEST_TREE_GRID.isTreeAtVisible(0, 3, 3)
      val bottomAtEdge = TEST_TREE_GRID.isTreeAtVisible(4, 0, 9)
      val rightAtEdge = TEST_TREE_GRID.isTreeAtVisible(3, 4, 9)
      val leftAtEdge = TEST_TREE_GRID.isTreeAtVisible(3, 0, 3)

      assertEquals(topAtEdge, true)
      assertEquals(bottomAtEdge, true)
      assertEquals(rightAtEdge, true)
      assertEquals(leftAtEdge, true)
    }

    test("Validate that the middle tree at 2,2 is invisible") {
      val middleTree = TEST_TREE_GRID.treeAt(2, 2)
      val shouldBeInvisible = TEST_TREE_GRID.isTreeAtVisible(2, 2, middleTree.height)

      assertEquals(shouldBeInvisible, false)
    }

    test("Tree in column 1 and row 3 should be visible from the right") {
      val isVisible = TEST_TREE_GRID.isVisibleInRow(2, 1, 5)

      assertEquals(isVisible, true)
    }

    test("Validate that a tree not at the edge should be visible from the right") {
      val isVisible = TEST_TREE_GRID.isTreeAtVisible(1, 1, 5)
      assertEquals(isVisible, true)
    }

  test("Validate that a tree not at the edge should be visible from the left") {
    val isVisible = TEST_TREE_GRID.isTreeAtVisible(3, 2, 5)
    assertEquals(isVisible, true)
  }

  test("Validate that a tree not at the edge should be visible from the bottom") {
    val isVisible = TEST_TREE_GRID.isTreeAtVisible(3, 2, 5)
    assertEquals(isVisible, true)
  }

  test("Validate that a tree not at the edge should be visible from the top") {
    val tree = TEST_TREE_GRID.treeAt(1, 2)
    val isVisible = TEST_TREE_GRID.isTreeAtVisible(1, 2, 5)
    assertEquals(isVisible, true)
  }

  test("COunt the number of visible trees should be 21") {
    val visibleTrees = TEST_TREE_GRID.countTreesVisible
    assertEquals(visibleTrees, 21)
  }

  test("test visibility score of a given row. 3, 2 is 4") {
    val rowScore = TEST_TREE_GRID.rowVisibilityScore(3, 2)

    assertEquals(rowScore, 4)
  }

  test("Visibility of a given column for tree 3, 2 is 2") {
    val colScore = TEST_TREE_GRID.colVisibilityScore(3, 2)

    assertEquals(colScore, 2)
  }

  test("Visibility of a given tree in 3, 2 is 8") {
    val visibilityScore = TEST_TREE_GRID.calculateVisibilityScore(3, 2)

    assertEquals(visibilityScore, 8)
  }

  test("Calculate All visibility scores") {
    val visibilityScores = TEST_TREE_GRID.calculateAllVisibilityScores

    val expectedResults = List(
      List(0, 0, 0, 0, 0),
      List(0, 1, 4, 1, 0),
      List(0, 6, 1, 2, 0),
      List(0, 1, 8, 3, 0),
      List(0, 0, 0, 0, 0)
    )

    assertEquals(visibilityScores, expectedResults)
  }

  test("Calculate the highest visibility scenic score should be 8") {
    val highestVisibilityScore = TEST_TREE_GRID.calculateHighestVisibilityScore

    assertEquals(highestVisibilityScore, 8)
  }

  test("Print visibility of each tree".ignore) {
    TEST_TREE_GRID.printVisibility
  }
}
