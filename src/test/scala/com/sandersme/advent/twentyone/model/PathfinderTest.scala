package com.sandersme.advent.twentyone.model

import com.sandersme.advent.twentyone.model.PathFinder.{LargeCave, Route, SmallCave, StartNode}

class PathfinderTest extends munit.FunSuite {
  val LARGE_TEST_INPUT = List("fs-end",
    "he-DX", "fs-he", "start-DX", "pj-DX", "end-zg", "zg-sl",
    "zg-pj", "pj-he", "RW-he", "fs-DX", "pj-RW", "zg-RW", "start-pj", "he-WI",
    "zg-he", "pj-fs", "start-RW")

  val SLIGHTLY_LARGE_TEST_INPUT = List("dc-end", "HN-start", "start-kj", "dc-start", "dc-HN", "LN-dc",
    "HN-end", "kj-sa", "kj-HN", "kj-dc")

  val TEST_SLIGHTLY_LARGE_NODES = PathFinder.parseInput(SLIGHTLY_LARGE_TEST_INPUT)

  val LARGE_TEST_NODES = PathFinder.parseInput(LARGE_TEST_INPUT)
  val TEST_INPUT: List[String] = List("start-A", "start-b", "A-c", "A-b", "b-d", "A-end", "b-end")
  val TEST_NODES = PathFinder.parseInput(TEST_INPUT)

  test("Validate we can parse adjacency List") {
    val testInput: List[String] = List("start-A", "start-b", "A-c", "A-b", "b-d", "A-end", "b-end")
    val pathMap = PathFinder.parseInput(testInput)

    val startConnectedNodes = pathMap(StartNode)
    val expectedStartNodeConnections = List(LargeCave("A"), SmallCave("b"))

    assertEquals(startConnectedNodes, expectedStartNodeConnections)
  }


  test("Route check if a small cave has been visited more than once") {
    val routeOneSmallCaveOnce = Route(SmallCave("a"), List(SmallCave("a")))
    val routeOneSmallCaveTwice = routeOneSmallCaveOnce
      .copy(path = List(SmallCave("a"), SmallCave("a")))
    val routeNewSmallCaveOneDuplicateSmallCaveVisited =
      Route(SmallCave("a"), List(SmallCave("b"), SmallCave("b")))

    val routeTwoSmallCavesAndExists = Route(
      SmallCave("a"), List(SmallCave("b"), SmallCave("b"), SmallCave("a"))
    )

    assertEquals(routeOneSmallCaveOnce.isVisitableSmallCave, true)
    assertEquals(routeOneSmallCaveTwice.isVisitableSmallCave, false)
    assertEquals(routeNewSmallCaveOneDuplicateSmallCaveVisited.isVisitableSmallCave, true)
    assertEquals(routeTwoSmallCavesAndExists.isVisitableSmallCave, false)
  }

  test("Find all paths through the cave. Small caves can only be traversed once.") {
    val allNodePaths = PathFinder.findPathsToEnd(TEST_NODES)
    assertEquals(allNodePaths.size, 36)
  }

  test("Find all paths for a LARGE input") {
    val allPaths = PathFinder.findPathsToEnd(LARGE_TEST_NODES)
    assertEquals(allPaths.size, 3509)
  }

  test("Find all paths in a slightly larger input") {
    val allPaths: List[Route] = PathFinder.findPathsToEnd(TEST_SLIGHTLY_LARGE_NODES)
    assertEquals(allPaths.size, 103)
  }
}
