package com.sandersme.advent.twentyone.model

import com.sandersme.advent.twentytwo.model.{HillClimbing, HillGrid, HillPoint, Node}

class HillClimbingTest extends munit.FunSuite {
  val TEST_INPUT = """Sabqponm
                     |abcryxxl
                     |accszExk
                     |acctuvwj
                     |abdefghi"""
    .stripMargin
    .split("\n")
    .toList

  val TEST_HILL_GRID: HillGrid = HillClimbing.parseInput(TEST_INPUT)


  test("Test that we can succesfully parse the TEST_INPUT") {
    val gridXSize = TEST_HILL_GRID.grid.size

    val gridYSize = TEST_HILL_GRID.grid.head.size

    val expectedGridXSize = 5
    val expectedGridYSize = 8

    assertEquals(expectedGridXSize, gridXSize)
    assertEquals(expectedGridYSize, gridYSize)
  }

  test("Find Starting Point") {
    val expectedStartingPoint = HillPoint(0, 0)

    val startingPoint = HillClimbing.findStartingNode(TEST_HILL_GRID).location

    assertEquals(startingPoint, expectedStartingPoint)
  }

  test("Find all neighbors") {
    val maxX = TEST_INPUT.size
    val maxY = TEST_INPUT.head.length
    val maxPoint = HillPoint(maxX, maxY)

    val testPointA = HillPoint(0, 0)
    val testPointB = HillPoint(100, 100)
    val testPointC = HillPoint(3, 8)
    val testPointD = HillPoint(4, 4)

    val resultsPointA = HillClimbing.findAllNeighbors(testPointA, maxPoint)
    val resultsPointB = HillClimbing.findAllNeighbors(testPointB, maxPoint)
    val resultsPointC = HillClimbing.findAllNeighbors(testPointC, maxPoint)
    val resultsPointD = HillClimbing.findAllNeighbors(testPointD, maxPoint)

    val expectedSizeA = 2
    val expectedSizeB = 0
    val expectedSizeC = 3
    val expectedSizeD = 4

    val expectedA = List(
      HillPoint(1, 0),
      HillPoint(0, 1)
    )

    assertEquals(resultsPointA.size, expectedSizeA)
    assertEquals(resultsPointB.size, expectedSizeB)
    assertEquals(resultsPointC.size, expectedSizeC)
    assertEquals(resultsPointD.size, expectedSizeD)
    assertEquals(resultsPointA, expectedA)
  }

  test("Find shortest path in a TEST_GRID should equal 31") {
    val shortestPath = HillClimbing.findShortestPath(TEST_HILL_GRID)
    val expectedShortestPath = 31

    assertEquals(shortestPath, expectedShortestPath)

  }

  test("Validate getNode val 0, 2 == b") {
    val node = TEST_HILL_GRID.getNode(HillPoint(0, 2))
    val expectedNode = Node(HillPoint(0, 2), 'b', List(HillPoint(0, 1), HillPoint(0, 3),
      HillPoint(1, 1), HillPoint(1, 2), HillPoint(1, 3)))

    assertEquals(node, expectedNode)
  }
}
