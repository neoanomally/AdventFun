package com.sandersme.advent.twentyfour

import scala.collection.mutable.ArrayBuilder.ofInt
import scala.annotation.experimental

class RaceConditionTest extends munit.FunSuite {

  test("Validate that we can parse the grid".ignore) {
    RACE_CONDITION.printGrid
  }

  test("find all neighbors to validate walls get filtered out") {
    val endNeighbors = RACE_CONDITION.neighborPoints(RACE_CONDITION.end)
    val startNeighbors = RACE_CONDITION.neighborPoints(RACE_CONDITION.start)
    val oneOneNeighbors = RACE_CONDITION.neighborPoints(Point(1, 1))
    val expectedEnd = List(Point(4, 7))
    val expectedStart = List(Point(1, 2))
    val expectedOneOne = List( Point(2, 1), Point(1, 2) )
    assertEquals(endNeighbors, expectedEnd) 
    assertEquals(startNeighbors,expectedStart)
    assertEquals(oneOneNeighbors, expectedOneOne)
  }

  test("Create a map that checks how many steps each point is from the end") {
    val map = RACE_CONDITION.endStepMap

    val expectedDistanceStart = 84

    assertEquals(map(RACE_CONDITION.start), expectedDistanceStart)
  }

  test("start looking at stepping twice to get closer to the end") {
    val map = RACE_CONDITION.endStepMap

    val teleportation = RACE_CONDITION.findTwoStepIgnoreWalls(map, Point(7, 7))

    val expectedTeleportation = Set(Point(5, 7), Point(9,7), Point(7, 5), Point(7, 9))
    assertEquals(teleportation, expectedTeleportation) 
  }


  test("EXPLORATION") {
    val f = RACE_CONDITION.findTwoStepsAllPoints(RACE_CONDITION.endStepMap)

    val agg = f.groupMapReduce(identity)(_ => 1)(_ + _)
      .toList.sortBy(_._1)
    
    val expected = List((2, 14), (4, 14), (6, 2), (8, 4), (10, 2),
      (12, 3), (20, 1), (36, 1), (38, 1), (40, 1), (64, 1))
    
    assertEquals(agg, expected)

  }


  val TEST_INPUT = """###############
                      #...#...#.....#
                      #.#.#.#.#.###.#
                      #S#...#.#.#...#
                      #######.#.#.###
                      #######.#.#...#
                      #######.#.###.#
                      ###..E#...#...#
                      ###.#######.###
                      #...###...#...#
                      #.#####.#.###.#
                      #.#...#.#.#...#
                      #.#.#.#.#.#.###
                      #...#...#...###
                      ###############"""
                .stripMargin.split("\n").toList.map(_.trim())

  val RACE_CONDITION = RaceCondition.parseGrid(TEST_INPUT)
}
