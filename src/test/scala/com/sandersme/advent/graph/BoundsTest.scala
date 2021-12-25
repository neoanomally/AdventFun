package com.sandersme.advent.graph

class BoundsTest extends munit.FunSuite {

  test("Bounds can parse input into a bounds object") {
    val bounds = Bounds.targetBoundsFromInput("target area: x=20..30, y=-10..-5")

    assertEquals(bounds.maxX, 30)
    assertEquals(bounds.minY, -10)
    assertEquals(bounds.maxY, -5)

  }

}
