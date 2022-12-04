package com.sandersme.advent.twentyone.model

import com.sandersme.advent.Input

class CuboidTest extends munit.FunSuite {
  val TEST_INPUT: List[String] = Input.readTwentyOneFromResource("day22_testinput")
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

  test("Cuboids that are overlapping. Calculate the distance overlap between them") {
    val cuboidA = Cuboid(10, 12, 10, 12, 10, 12)
    val cuboidB = Cuboid(9, 11, 9, 11, 9, 11)
    val cuboidC = Cuboid(9, 11, 9, 11, 5, 7)


    assertEquals(Cuboid.intersection(cuboidA, cuboidB).isDefined, true)
    assertEquals(Cuboid.intersection(cuboidA, cuboidC).isDefined, false)
  }

  test("Add two cuboids") {
    val cuboidA = Cuboid(10, 12, 10, 12, 10, 12)
    val cuboidB = Cuboid(9, 11, 9, 11, 9, 11)
    val expectedCuboidIntersectionAB: Cuboid = Cuboid(9, 12, 9, 12, 9, 12)

    assertEquals(Cuboid.add(cuboidA, cuboidB), expectedCuboidIntersectionAB)
  }

  test("Validate volume of different sized cuboids") {
    val cuboidOne = Cuboid(10, 11, 10, 11, 1, 2)

    assertEquals(cuboidOne.volume, 8)
  }
}
