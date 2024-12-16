package com.sandersme.advent.twentyfour

class WarehouseWoesTest extends munit.FunSuite {
  test("TEST that we can parse out the locations of boxes and robot") {
    val expectedRobotLoc = Point(4, 4)

    val pointOneWall = TEST_WAREHOUSE.objectAt(0, 0)
    val pointTwoBox = TEST_WAREHOUSE.objectAt(4, 2)
    val pointThreeSpace = TEST_WAREHOUSE.objectAt(1, 1)

    assertEquals(pointThreeSpace, Open)
    assertEquals(pointOneWall, Wall)
    assertEquals(pointTwoBox, Open)
    assertEquals(TEST_WAREHOUSE.robot, expectedRobotLoc)
  }

  test("validate that we can move all objects for part two") {
    val expand = TEST_WAREHOUSE.expandGridPartTwo
    // println("\nExpanding the grid:")
    // WarehouseWoes.printPartTwo(expand.objects, expand.robot)
    val afterMoved = expand.moveRobotPartTwo
    // println("Part two moving across grive")
    // WarehouseWoes.printPartTwo(afterMoved.objects, afterMoved.robot)
  }

  test("validate the final score of the grid at the end.") {
    val finalWarehouse = TEST_WAREHOUSE.moveRobotPartOne

    finalWarehouse.printGrid

    val finalScore = finalWarehouse.calculateFinalScore
    
    assertEquals(finalScore, 10092L)
  }

  test("Validate that we can move all te boxes for troubleshooting purposes".ignore) {
    // println("BEFORE MOVING:")
    // TEST_WAREHOUSE.printGrid
    // println("\n\nAFter Moving Grid")
    // TEST_WAREHOUSE.moveRobotPartOne.printGrid
  }

  test("Validate score for exapanded grid for part 2 should equal 9021") {
    val expanded = TEST_WAREHOUSE.expandGridPartTwo.moveRobotPartTwo.calculateFinalScorePartTwo

    assertEquals(expanded, 9021L)
    
  }

  val TEST_INPUT = """##########
                      #..O..O.O#
                      #......O.#
                      #.OO..O.O#
                      #..O@..O.#
                      #O#..O...#
                      #O..O..O.#
                      #.OO.O.OO#
                      #....O...#
                      ##########

      <vv>^<v^>v>^vv^v>v<>v^v<v<^vv<<<^><<><>>v<vvv<>^v^>^<<<><<v<<<v^vv^v>^
      vvv<<^>^v^^><<>>><>^<<><^vv^^<>vvv<>><^^v>^>vv<>v<<<<v<^v>^<^^>>>^<v<v
      ><>vv>v^v^<>><>>>><^^>vv>v<^^^>>v^v^<^^>v^^>v^<^v>v<>>v^v^<v>v^^<^^vv<
      <<v<^>>^^^^>>>v^<>vvv^><v<<<>^^^vv^<vvv>^>v<^^^^v<>^>vvvv><>>v^<<^^^^^
      ^><^><>>><>^^<<^^v>>><^<v>^<vv>>v>>>^v><>^v><<<<v>>v<v<v>vvv>^<><<>^><
      ^>><>^v<><^vvv<^^<><v<<<<<><^v<<<><<<^^<v<^^^><^>>^<v^><<<^>>^v<v^v<v^
      >^>>^v>vv>^<<^v<>><<><<v<<v><>v<^vv<<<>^^v^>^^>>><<^v>>v^v><^^>>^<>vv^
      <><^^>^^^<><vvvvv^v<v<<>^v<v>v<<^><<><<><<<^^<<<^<<>><<><^^^>^^<>^>v<>
      ^^>vv<^v^v<vv>^<><v<^v>^^^>>>^^vvv^>vvv<>>>^<^>>>>>^<<^v>^vvv<>^<><<v>
      v^^>>><<^^<>>^v^<v^vv<>v^<<>^<^v^v><^<<<><<^<v><v<>vv>>v><v^<vv<>v^<<^"""
    .stripMargin
    .split("\n")
    .toList
    .map(_.trim)

    val TEST_WAREHOUSE = WarehouseWoes.parseInput(TEST_INPUT)
}


/**
  *
  *
##########
#.O.O.OOO#
#........#
#OO......#
#OO@.....#
#O#.....O#
#O.....OO#
#O.....OO#
#OO....OO#
##########


####################
##[].......[].[][]##
##[]...........[].##
##[]........[][][]##
##[]......[]....[]##
##..##......[]....##
##..[]............##
##..@......[].[][]##
##......[][]..[]..##
####################

  */
