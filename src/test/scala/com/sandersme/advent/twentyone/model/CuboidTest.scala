package com.sandersme.advent.twentyone.model

import com.sandersme.advent.twentyone.Input

class CuboidTest extends munit.FunSuite {

  test("parse the test input") {
    val testInput = Input.readFromDataResource("day21_testinput")

    val cuboids = Cuboid.parseInput(testInput)

    assertEquals(cuboids.size, 22)

  }

}
