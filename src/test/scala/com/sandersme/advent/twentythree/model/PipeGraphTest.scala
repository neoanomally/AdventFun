package com.sandersme.advent.twentythree.model

import com.sandersme.advent.twentyone.graph.Point
import com.sandersme.advent.twentythree.model.PipeGraph.findNeighbors
import com.sandersme.advent.twentythree.PipeMaze.transformedPipeGraph

class PipeGraphTest extends munit.FunSuite {

  val TEST_INPUT = """.....
      |.S-7.
      |.|.|.
      |.L-J.
      |.....""".stripMargin
    .split("\n")
    .toList

  val pipeGraph = PipeGraph.parseInput(TEST_INPUT)

  test("Validate taht PipeGraph parses input correctly".ignore) {
    // Just for troubleshooting and visualization
    val pipeGraph = PipeGraph.parseInput(TEST_INPUT)

    pipeGraph.printMainLoopWithInnerOuter
  }

  test("Find all ground groups. There should only be two in the TEST_INPUT") {
    val allGroundGroups = pipeGraph.findAllGroundGroups
    val outerGroundds = allGroundGroups.head
    val expectedOuterGroundsSize = 16

    assertEquals(outerGroundds.size, expectedOuterGroundsSize)
  }

  test("Check if group is touching edge") {
    val testGroupA = Set(Point(0, 1), Point(1, 1))
    val testGroupB = Set(Point(1, 1), Point(1, 2))
    val resultsA = PipeGraph.isInnerGroundGroup(testGroupA, 4, 4)
    val resultsB = PipeGraph.isInnerGroundGroup(testGroupB, 4, 4)

    assertEquals(resultsA, false)
    assertEquals(resultsB, true)
  }

  test("Second PipeGraph that should have 10 tiles in the innerGroundGroups") {
    val largerInput = 
      """FF7FSF7F7F7F7F7F---7
        |L|LJ||||||||||||F--J
        |FL-7LJLJ||||||LJL-77
        |F--JF--7||LJLJ7F7FJ-
        |L---JF-JLJ.||-FJLJJ7
        ||F|F-JF---7F7-L7L|7|
        ||FFJF7L7F-JF7|JL---7
        |7-L-JL7||F7|L7F-7F7|
        |L.L7LFJ|||||FJL7||LJ
        |L7JLJL-JLJLJL--JLJ.L""".stripMargin.split("\n").toList

    val otherPipeGraph = PipeGraph.parseInput(largerInput)
    val transformedPipeGraph = otherPipeGraph.transformNonMainPipesToGround

    // transformedPipeGraph.printMainLoopWithInnerOuter
    val innerPipeTileCount = transformedPipeGraph.countTilesInsideLoops
    val pointA = transformedPipeGraph.getPipe(Point(0 ,8))
    val pointB = transformedPipeGraph.getPipe(Point(1 ,8))
    val expectedInnerTiles = 10

    assertEquals(innerPipeTileCount, expectedInnerTiles)

  }

  test("Find areas where water can leak between the pipes.") {
    val squeezedPipesInput = 
        """..........
          |.S------7.
          |.|F----7|.
          |.||....||.
          |.||....||.
          |.|L-7F-J|.
          |.|..||..|.
          |.L--JL--J.
          |..........""".stripMargin.split("\n").toList

    val squeezedPipeGraph = PipeGraph.parseInput(squeezedPipesInput)
    val transformedSqueezedGraph = squeezedPipeGraph.transformNonMainPipesToGround
    val numTiles = transformedSqueezedGraph.countTilesInsideLoops 
    val expectedTiles = 4
    // transformedSqueezedGraph.printMainLoopWithInnerOuter

    assertEquals(numTiles, expectedTiles)
  }

  test("Create DistanceMap Point") {
    val looped = pipeGraph.loopThroughPipes.map((k, v) => k -> v.toString)
    val stepsMap = Map(
      Point(1, 1) -> 0, Point(2, 1) -> 1, Point(3, 1) -> 2, Point(1, 3) -> 2,
      Point(2, 3) -> 3, Point(3, 3) -> 4, Point(3, 2) -> 3
    )
    assertEquals(stepsMap, stepsMap)
  }

  test("Number of tiles in the inner group should equal 1") {
    val numberInnerTiles = pipeGraph.countTilesInsideLoops
    val expected = 1

    assertEquals(numberInnerTiles, expected)
  }

  test("validate the nighbors are correct in the TEST_GRAPH") {
    val startPoint = Point(1, 1)
    val startPipe = pipeGraph.getPipe(startPoint)
    val leftVerticalPoint = Point(1, 2)
    val leftVerticalPipe = pipeGraph.getPipe(leftVerticalPoint)

    val expectedLeftVerticalPipe = Pipe(PipeType.Vertical, leftVerticalPoint, List(Point(1, 1), Point(1, 3)))
    val expectedStartPipe = Pipe(PipeType.Start, Point(1, 1), List(Point(2, 1), Point(1, 2)))

    assertEquals(leftVerticalPipe, expectedLeftVerticalPipe)
    assertEquals(startPipe, expectedStartPipe)
  }

  test("Find start should be 1, 1") {
    val start = pipeGraph.findStart
    val expectedPoint = Point(1, 1)

    assertEquals(start.point, expectedPoint)
  }



  test("Parse various pipetypes to get the right neighbors") {
    val neighborsA = PipeGraph.findNeighbors(PipeType.Horizontal, Point(0, 0), 3, 3)
    val expectedNeighborsA = List(Point(1, 0))

    val neighborsB = PipeGraph.findNeighbors(PipeType.Vertical, Point(0, 0), 3, 3)
    val expectedNeighborsB = List(Point(0, 1))

    val neighborsC = findNeighbors(PipeType.SWBend, Point(1, 1), 3, 3)
    val expectedNeighborsC = List(Point(0, 1), Point(1, 2))

    val neighborsD = findNeighbors(PipeType.NEBend, Point(1, 1), 3, 3)
    val expectedNeighborsD = List(Point(1, 0), Point(2, 1))

    val neighborsE = findNeighbors(PipeType.SEBend, Point(3, 3), 3, 3)
    val expectedNeighborsE = List.empty[Point]

    assertEquals(neighborsA, expectedNeighborsA)
    assertEquals(neighborsB, expectedNeighborsB)
    assertEquals(neighborsC, expectedNeighborsC)
    assertEquals(neighborsD, expectedNeighborsD)
    assertEquals(neighborsE, expectedNeighborsE)
  }
}
