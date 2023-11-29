package com.sandersme.advent.twentyone.model

import com.sandersme.advent.twentytwo.model.{HillClimbing, HillGrid, HillPoint, Node, NodeType}

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
    val gridXSize = TEST_HILL_GRID.grid.head.size
    val gridYSize = TEST_HILL_GRID.grid.size

    val expectedGridXSize = 8
    val expectedGridYSize = 5

    assertEquals(expectedGridXSize, gridXSize)
    assertEquals(expectedGridYSize, gridYSize)
  }

  test("Find all neighbors") {
    val maxX = TEST_INPUT.length
    val maxY = TEST_INPUT.head.length
    val maxPoint = HillPoint(maxX, maxY)
    val expectedMaxPoint = HillPoint(5, 8)


    assertEquals(maxPoint, expectedMaxPoint)

    val testPointA = HillPoint(0, 0)
    val testPointB = HillPoint(100, 100)
    val testPointC = HillPoint(3, 7)
    val testPointD = HillPoint(4, 4)

    val resultsPointA = HillClimbing.findAllNeighbors(testPointA, maxPoint)
    val resultsPointB = HillClimbing.findAllNeighbors(testPointB, maxPoint)
    val resultsPointC = HillClimbing.findAllNeighbors(testPointC, maxPoint)
    val resultsPointD = HillClimbing.findAllNeighbors(testPointD, maxPoint)

    val expectedSizeA = 2
    val expectedSizeB = 0
    val expectedSizeC = 3
    val expectedSizeD = 3

    val expectedA = List(
      HillPoint(1, 0),
      HillPoint(0, 1)
    )

    val expectedD = List(
      HillPoint(3, 4),
      HillPoint(4, 5),
      HillPoint(4, 3)
    )

    assertEquals(resultsPointA.size, expectedSizeA)
    assertEquals(resultsPointB.size, expectedSizeB)
    assertEquals(resultsPointC.size, expectedSizeC)
    assertEquals(resultsPointD.size, expectedSizeD)
    assertEquals(resultsPointA, expectedA)
    assertEquals(resultsPointD, expectedD)
  }

  test("Validate that the Ending node is 5, 2") {
    val endingNode: Node = TEST_HILL_GRID.findEndingNode

    val expectedValue = 'z'
    val expectedLocation = HillPoint(5, 2)

    assertEquals(endingNode.location, expectedLocation)
    assertEquals(endingNode.value, expectedValue)
  }

  test("Validate that the Starting node is 0, 0") {
    val startingNode: Node = TEST_HILL_GRID.findStartingNode

    val expectedValue = 'a'
    val expectedLocation = HillPoint(0, 0)

    assertEquals(startingNode.location, expectedLocation)
    assertEquals(startingNode.value, expectedValue)
  }

  test("Validate we can find the shortest path to the destination node in 31 steps") {
    val shortestPath = HillClimbing.findShortestPathFromStart(TEST_HILL_GRID)
    val expectedDistance = 31

    assertEquals(shortestPath, expectedDistance)
  }


  test("Validate getNode val 0, 2 == b") {
    val node = TEST_HILL_GRID.getNode(HillPoint(2, 0))
    val expectedNode = Node(HillPoint(2, 0), 'b', List(HillPoint(1, 0), HillPoint(3, 0),
      HillPoint(2, 1)), NodeType.Other)

    assertEquals(node, expectedNode)
  }

  test("Validate we can start at the ending node and find the minimum distance to any a") {
    val shortestPath = HillClimbing.findShortestAFromEndingNode(TEST_HILL_GRID)
    val expectedShortestPath = 29

    assertEquals(shortestPath, expectedShortestPath)
  }

}
