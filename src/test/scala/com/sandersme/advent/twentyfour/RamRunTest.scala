package com.sandersme.advent.twentyfour

class RamRunTest extends munit.FunSuite {
  test("Find minimum number of steps for the test case") {
    val minSteps = TEST_RAM_RUN.advanceBytes(12).findMinNumberSteps


    assertEquals(minSteps, 22)
  }

  test("check BFSCanmakeIt fully advanced and 12 bytes") {
    val twelveBytes = TEST_RAM_RUN.advanceBytes(12).startCanMakeIt
    val fullyBytes = TEST_RAM_RUN.advanceBytes(1024).startCanMakeIt

    assertEquals(twelveBytes, true)
    assertEquals(fullyBytes, false)
  }

  test("Do a binary search to find the index of the last byte") {
    val binarySearch = RamRun.binarySearchFirstPoint(TEST_RAM_RUN)

    assertEquals(binarySearch, Point(6, 1))
  }

  val TEST_INPUT = """5,4
      4,2
      4,5
      3,0
      2,1
      6,3
      2,4
      1,5
      0,6
      3,3
      2,6
      5,1
      1,2
      5,5
      2,5
      6,5
      1,4
      0,4
      6,4
      1,1
      6,1
      1,0
      0,5
      1,6
      2,0""".stripMargin.split("\n").toList.map(_.trim())

    val TEST_RAM_RUN = RamRun.parseInput(TEST_INPUT, 7, 7)
}
