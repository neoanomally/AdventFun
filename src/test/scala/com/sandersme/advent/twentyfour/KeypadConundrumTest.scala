package com.sandersme.advent.twentyfour

import scala.collection.mutable.{Map => MMap }

class KeypadConundrumTest extends munit.FunSuite {
  test("Test creating the graph and verifying points are correct") {
    val testGraph = KeypadConundrum.createKeyPoints(NumericKeypadGraph)

    assertEquals(testGraph.valueLookup('7'), Point(0, 0))
    assertEquals(testGraph.valueLookup('0'), Point(1, 3))
  }


  test("Test taht we can create a path fora ny given point to all other points") {
    val testGraph = KeypadConundrum.createKeyPoints(NumericKeypadGraph)
    val point = Point(0, 0)
    
    val shortestPath = KeypadConundrum.calculateShortestPath(List(KeyPath('7', '-', List.empty)), testGraph, Map.empty )

    val eightShortest = shortestPath('8')
    val expected = List(List('>'))

    assertEquals(eightShortest, expected)
  }

  test("Create shortest path with the directional input") {
    val testGraph = KeypadConundrum.createKeyPoints(DirectionalKeyPad)

    val point = Point(1, 0)

    val shortestPath = KeypadConundrum.calculateShortestPath(List(KeyPath('^', '-', List.empty)), testGraph, Map.empty)
    val leftArrowShortest = shortestPath('<')
    val expected = List(List('v', '<')) 

    assertEquals(leftArrowShortest, expected)
  }

  test("Test the length of a path between two points on keymap") {
    val shortestPath = KEYBOARD_CONUNDRUM.calculateFirstPath("029A")
    val expected = List('<', 'A', '^', 'A', '>', '^', '^', 'A', 'v', 'v', 'v', 'A')

    assertEquals(shortestPath.head, expected)
  }

  test("Calculate ShortestPath for second robot") {
    val input = List('<', 'A', '^', 'A', '>', '^', '^', 'A', 'v', 'v', 'v', 'A')
    val shortestPath = KEYBOARD_CONUNDRUM.calculatePath(input, KEYBOARD_CONUNDRUM.directionPaths, MMap.empty)

    val shouldExist = shortestPath.exists( c => c.mkString == "v<<A>>^A<A>AvA<^AA>A<vAAA>^A")
    assertEquals(shouldExist, true)
  }

  /// TODO: UNIGNORE ONCE I FIND THE cahce method
  test("TESTING MYH SANITY this will be ignored due to how much it costs".ignore) {
    val shortest = KEYBOARD_CONUNDRUM.calculateShortestPathCosts

    assertEquals(shortest, 126384L)
  }

  test("TESTING MY SANITY PART TWO WILL BE IGNORED") {
    val shortest = KEYBOARD_CONUNDRUM.copy(numRobots = 2, instructions = List("029A"))
      .calculateCostPartTwo

    val expectedResult = 68 * 29L

    assertEquals(shortest, expectedResult)
  }
  test("TESTING MY SANITY PART TWO PART TWO PART TWO ") {
    val shortest = KEYBOARD_CONUNDRUM.copy(numRobots = 2, instructions = List("980A"))
      .calculateCostPartTwo

    val expectedResult = 60L * 980L

    assertEquals(shortest, expectedResult)
  }

  test("Test all combinations for apart one using the second solution".ignore) {
    val shortest = KEYBOARD_CONUNDRUM.copy(numRobots = 2)
      .calculateCostPartTwo

    println("SHortest: " + shortest)
  }

// <vA<AA>>^AvAA<^A>A<v<A>>^AvA^A<vA>^A<v<A>^A>AAvA^A<v<A>A>^AAAvA<^A>A
// v<<A   >>^A     <A >A vA <^A A >A <vA A A>^A
// <A     ^A      >^^A vvvA
// 
  val TEST_INPUT = """029A
                      980A
                      179A
                      456A
                      379A""".stripMargin
                      .split("\n")
                      .toList
                      .map(_.trim)

  val KEYBOARD_CONUNDRUM = KeypadConundrum.parseInput(TEST_INPUT)
}
