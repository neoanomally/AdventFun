package com.sandersme.advent.twentyone.model

import com.sandersme.advent.twentyone.Input

class CuboidTest extends munit.FunSuite {
  val TEST_INPUT: List[String] = Input.readFromDataResource("day22_testinput")
  val CUBOIDS: Cuboids = Cuboid.parseInput(TEST_INPUT)

  test("parse the test input") {
    assertEquals(CUBOIDS.size, 22)
    assertEquals(CUBOIDS.head, Cuboid(-20, 26, -36, 17, -47, 7, true))
  }

  test("process cuboids for part 1") {
    val cuboidsToProcess = CUBOIDS.dropRight(2)
    val coordinates = Cuboid.processCuboids(cuboidsToProcess)

    assertEquals(coordinates.size, 590784)
  }

}
