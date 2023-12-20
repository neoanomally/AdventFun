package com.sandersme.advent.twentythree.model

import com.sandersme.advent.twentyone.graph.Point

class GalaxyImageTest extends munit.FunSuite {
  val TEST_INPUT = """...#......
        |.......#..
        |#.........
        |..........
        |......#...
        |.#........
        |.........#
        |..........
        |.......#..
        |#...#.....""".stripMargin.split("\n").toList
  
  test("Validate that we can parse the test iMage") {
    val galaxy = GalaxyImage.parseInput(TEST_INPUT)

    val expectedPoints = Vector(
      Point(3, 0), Point(7, 1), Point(0, 2), Point(6, 4), Point(1, 5),
      Point(9, 6), Point(7, 8), Point(0, 9), Point(4, 9)
    )

    assertEquals(galaxy.galaxyPoints, expectedPoints)
  }

  test("Expand the galaxy has the correct number of spaces") {
    val expandedGalexy = GalaxyImage.parseInput(TEST_INPUT).expandGalaxy

    val expectedExpansion = Vector(

      Point(5, 0), Point(10, 1), Point(0, 2), Point(8, 5), Point(1, 6),
      Point(12, 7), Point(10, 9), Point(0, 11), Point(5, 11)
    )
  }

  test("Expand the galaxy then find every pair of points")  {
    val expandedGalaxy = GalaxyImage.parseInput(TEST_INPUT).expandGalaxy()

    val pairOfPoints = expandedGalaxy.findEveryPointsPair

    val numPairs = pairOfPoints.size
    val expectedNumPairs = 36
    assertEquals(numPairs, expectedNumPairs)
  }

  test("Calculate distance between two pairs is correct") {
    val left = Point(4, 0)
    val right = Point(10, 9)

    val expectedDistance = 15L
    val distance = GalaxyImage.calculateDistanceBetweenPoints(left, right)

    assertEquals(distance, expectedDistance)
  }

  test("Calculate the sum of the shortest distance path between pairs equals 374") {
    val expandedGalaxy = GalaxyImage.parseInput(TEST_INPUT).expandGalaxy()

    val sumOfDistance = expandedGalaxy.sumDistanceEveryPair
    val expectedSumOfDistance = 374L

    assertEquals(sumOfDistance, expectedSumOfDistance)

  }

  test("Calculate the sum of the shortest distance by adding a factor of a million") {
    val expandedGalaxyFactor = GalaxyImage.parseInput(TEST_INPUT).expandGalaxy(10)

    val sumOfDistance = expandedGalaxyFactor.sumDistanceEveryPair
    val expectedSumDistance = 1030L

    val largerExapndedFactor = GalaxyImage.parseInput(TEST_INPUT)
      .expandGalaxy(100)
      .sumDistanceEveryPair

    val expectedLargerSumDistance = 8410L

    assertEquals(sumOfDistance, expectedSumDistance)
    assertEquals(largerExapndedFactor, expectedLargerSumDistance)
  }
}
