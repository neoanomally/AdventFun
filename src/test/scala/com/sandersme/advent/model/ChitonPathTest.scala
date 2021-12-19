package com.sandersme.advent.model

import com.sandersme.advent.graph.Point

import scala.collection.mutable

class ChitonPathTest extends munit.FunSuite {

  val TEST_INPUT = List(
    "1163751742",
    "1381373672",
    "2136511328",
    "3694931569",
    "7463417111",
    "1319128137",
    "1359912421",
    "3125421639",
    "1293138521",
    "2311944581"
  )

  val TEST_CHITON = ChitonPath.parseInput(TEST_INPUT)

  test("Validate that we can parse the input into a MxM grid") {
    val chitonPath: ChitonPath = ChitonPath.parseInput(TEST_INPUT)
    assertEquals(chitonPath.matrix(2)(3).value, 6)
  }

  test("Test the shortest Path between start and end") {
    val fastestRoute = ChitonPath.findShortestRoute(TEST_CHITON)

    assertEquals(fastestRoute.distanceToEnd, Option(40))
  }

}
