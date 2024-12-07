package com.sandersme.advent.twentyfour

import com.sandersme.advent.twentytwo.model.CrateSupply.moveForwardOneInstruction

class GuardGallivantTest extends munit.FunSuite {
  test("Validate that we can parse the input where we have guard position and obstacle positions") {
    val guardGallivant = GuardGallivant.parseInput(TEST_INPUT)
    
    val guardPos = guardGallivant.guard
    assertEquals(guardPos, Point(4, 6))
    
    val firstRow = guardGallivant.rows.getOrElse(0, List.empty)
    assertEquals(firstRow, List(4))

    val firstCol = guardGallivant.cols.getOrElse(0, List.empty)
    assertEquals(firstCol, List(8))

    assertEquals(guardGallivant.direction, Direction.Up)
  }

  test("Validate that the guard starts on the grid.") {
    assertEquals(TEST_GUARD_GRID.isFinished, false)
  }

  test("TEST THE HEIGHT AND WIDTH") {
    
    assertEquals(TEST_GUARD_GRID.height, 10)
    assertEquals(TEST_GUARD_GRID.width, 10)
  }

  test("Test that we can move the guard forward 1 postion should be at point(4, 1) and Direction Right") {
    val moveOne = TEST_GUARD_GRID.moveGuard
    val moveTwo = moveOne.moveGuard
    val moveThree = moveTwo.moveGuard
    val moveFour = moveThree.moveGuard

    assertEquals(moveOne.guard, Point(4, 1))
    assertEquals(moveOne.direction, Direction.Right)

    assertEquals(moveTwo.direction, Direction.Down)
    assertEquals(moveTwo.guard, Point(8, 1))

    assertEquals(moveThree.direction, Direction.Left)
    assertEquals(moveThree.guard, Point(8, 6))

    assertEquals(moveFour.direction, Direction.Up)
    assertEquals(moveFour.guard, Point(2, 6))
  }

  test("Move the guard till its offboard") {
    val finalGuard = GuardGallivant.moveGuardTillOffboard(TEST_GUARD_GRID)

    assertEquals(finalGuard.guard, Point(-1, -1))
    assertEquals(finalGuard.visitedPoints.size, 41) 
  }
  
  test("Count the number of loops to add obstacles to".ignore) {
    val finalGuard = GuardGallivant.moveGuardTillOffboard(TEST_GUARD_GRID)

    val numLoops = finalGuard.countNumLoops

    assertEquals(numLoops, 6)
  }

  val TEST_INPUT = """....#.....
      .........#
      ..........
      ..#.......
      .......#..
      ..........
      .#..^.....
      ........#.
      #.........
      ......#..."""
        .stripMargin
        .split("\n")
        .toList
        .map(_.trim())


  val TEST_GUARD_GRID = GuardGallivant.parseInput(TEST_INPUT)
}
