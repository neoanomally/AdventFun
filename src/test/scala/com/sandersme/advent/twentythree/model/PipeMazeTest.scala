package com.sandersme.advent.twentythree

import com.sandersme.advent.twentythree.MPipe.*
import com.sandersme.advent.twentythree.PipeMazeV2.canConnect
import com.sandersme.advent.twentythree.CosmicExpansion.input

class PipeMazeTest extends munit.FunSuite {

  test("Test to make sure that the maze is properly parsed") {

    assertEquals(TEST_PIPE.maze(0)(1), MPipe.H)

  }

  test("Test that the furthest step is 8 away") {
    val furthest = TEST_PIPE.bfsFurthestStep
    assertEquals(furthest, 8)
  }

  test("Test that pipes can only connect to valid neighbors") {
    val startRightTrue = canConnect(MPipe.START, MPipe.NW, Dir.E)
    val startRightFalse = canConnect(MPipe.START, MPipe.NE, Dir.E)
    val startNEConnectsSETrue = canConnect(MPipe.NE, MPipe.SE, Dir.N)
    val norhtEConnectSWTrue = canConnect(MPipe.NE, MPipe.SW, Dir.E)
    val norhtEConnectNEFalse = canConnect(MPipe.NE, MPipe.NE, Dir.E)
    val startCanConnectVertical = canConnect(MPipe.START, MPipe.V, Dir.S)

    assertEquals(startRightTrue, true)
    assertEquals(startRightFalse, false)
    assertEquals(startNEConnectsSETrue, true)
    assertEquals(norhtEConnectSWTrue, true)
    assertEquals(norhtEConnectNEFalse, false)
    assertEquals(startCanConnectVertical, true)
  }

  test("Find two neighbors from the startingPoint") {
    val startNeighbors = TEST_PIPE.findTwoNeighbors(0, 2)
    val oneOneNeighbros = TEST_PIPE.findTwoNeighbors(1, 1)
    val twoTwoNeighbors = TEST_PIPE.findTwoNeighbors(2, 2)
    
    assertEquals(startNeighbors.size, 2)
    assertEquals(startNeighbors.last, (0, 3))
    assertEquals(oneOneNeighbros.size, 2)
    assertEquals(twoTwoNeighbors.size, 0)
  }

  test("Test that we can flood the outside of the board") {
    val floodedBoard = TEST_PIPE.floodBoard  

    println(floodedBoard)
  }

  val TEST_INPUT = """7-F7-
                      .FJ|7
                      SJLL7
                      |F--J
                      LJ.LJ""".split("\n").toList.map(_.trim)
  val TEST_PIPE = PipeMazeV2.parseInput(TEST_INPUT)


  val TEST_INPUT2 =  """.F----7F7F7F7F-7....
                        .|F--7||||||||FJ....
                        .||.FJ||||||||L7....
                        FJL7L7LJLJ||LJ.L-7..
                        L--J.L7...LJS7F-7L7.
                        ....F-J..F7FJ|L7L7L7
                        ....L7.F7||L7|.L7L7|
                        .....|FJLJ|FJ|F7|.LJ
                        ....FJL-7.||.||||...
                        ....L---J.LJ.LJLJ...""".split("\n").toList.map(_.trim)

  val TEST_PIPE2 = PipeMazeV2.parseInput(TEST_INPUT2)
}
