package com.sandersme.advent.twentyone.graph

class GridTest extends munit.FunSuite {

  test("Check generating all points") {
    val generatedValues = Grid.generateNeighborValues(1, 1, 10, 10)
    val generatedValuesZeroZero = Grid.generateNeighborValues(0, 0, 10, 10)
    val generatedValuesElevens = Grid.generateNeighborValues(11, 11, 10, 10)
    val generatedValuesTens = Grid.generateNeighborValues(10, 10, 10, 10)

    assertEquals(generatedValues.size, 8)
    assertEquals(generatedValues.contains(Point(1, 1)), false)
    assertEquals(generatedValuesZeroZero.size, 3)
    assertEquals(generatedValuesElevens.size, 0)
    assertEquals(generatedValuesTens.size, 1)
  }
}
