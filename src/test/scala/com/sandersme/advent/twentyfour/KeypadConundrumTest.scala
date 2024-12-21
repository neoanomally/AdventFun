package com.sandersme.advent.twentyfour

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
    val expected = List(List('-'))

    assertEquals(eightShortest, expected)
  }

  test("Create shortest path with the directional input") {
    val testGraph = KeypadConundrum.createKeyPoints(DirectionalKeyPad)

    val point = Point(1, 0)

    val shortestPath = KeypadConundrum.calculateShortestPath(List(KeyPath('^', '-', List.empty)), testGraph, Map.empty)
    val leftArrowShortest = shortestPath('<')
    val expected = List(List('-', 'v')) 

    assertEquals(leftArrowShortest, expected)
  }

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
