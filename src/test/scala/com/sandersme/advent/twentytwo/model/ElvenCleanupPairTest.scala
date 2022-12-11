package com.sandersme.advent.twentytwo.model

import com.sandersme.advent.twentytwo.model

class ElvenCleanupPairTest extends munit.FunSuite {
  val ALL_CLEANUP_PAIRS = List("2-4,6-8", "2-3,4-5", "5-7,7-9", "2-8,3-7", "6-6,4-6", "2-6,4-8")

  test("Parse all the assignments into ElvenCleanUp Pairs into 6 assignments") {
    val assignments: Seq[model.ElvenCleanupPair] = ElvenCleanupPair.fromInput(ALL_CLEANUP_PAIRS)
  }

  test("Parse a single input: \"2-4,6-8\" in to elvenPairRange") {
    val elvenCleanupPair = ElvenCleanupPair.parseLine("2-4,6-8")
    val expectedCleanupPair = ElvenCleanupPair(CleanupRange(2, 4), CleanupRange(6, 8))

    assertEquals(elvenCleanupPair, expectedCleanupPair)
  }

  test("The right cleanup elf's route overlaps with the left cleanup elf") {
    val left = CleanupRange(5, 10)
    val right = CleanupRange(6, 9)

    val elvenCleanupPair = ElvenCleanupPair(left, right)

    assertEquals(elvenCleanupPair.containsFullyOverlappingCleanup, true)
  }


  test("The right cleanup elf's route should not overlap with the left cleanup elf") {
    val left = CleanupRange(5, 10)
    val right = CleanupRange(6, 11)

    val elvenCleanupPair = ElvenCleanupPair(left, right)

    assertEquals(elvenCleanupPair.containsFullyOverlappingCleanup, false)
  }

  test("There should be zero overlaps on the elven cleanup pair") {
    val left = CleanupRange(5, 10)
    val right = CleanupRange(11, 19)

    val elvenCleanupPair = ElvenCleanupPair(left, right)

    assertEquals(elvenCleanupPair.containsFullyOverlappingCleanup, false)
  }

  test("Check to see if values are partially overlapping in the elven Cleanu Pair true") {
    val left = CleanupRange(6, 10)
    val right = CleanupRange(8, 12)

    val testOneElvenCleanupPair = ElvenCleanupPair(left, right)
    val testTwoElvenCleanupPair = ElvenCleanupPair(right, left)

    assertEquals(testOneElvenCleanupPair.containsPartiallyOverlappingCleanup, true)
    assertEquals(testTwoElvenCleanupPair.containsPartiallyOverlappingCleanup, true)
  }

  test("containsPartiallyOverlappingCleanup should return true if the range is 6, 10 & 10, 15") {
    val left = CleanupRange(6, 10)
    val right = CleanupRange(10, 15)

    val testOneElvenCleanupPair = ElvenCleanupPair(left, right)
    val testTwoElvenCleanupPair = ElvenCleanupPair(right, left)

    assertEquals(testOneElvenCleanupPair.containsPartiallyOverlappingCleanup, true)
    assertEquals(testTwoElvenCleanupPair.containsPartiallyOverlappingCleanup, true)
  }

  test("containsPartiallyOverlappingCleanup should return false if the range is 6, 10 & 11, 15") {
    val left = CleanupRange(6, 10)
    val right = CleanupRange(11, 15)

    val testOneElvenCleanupPair = ElvenCleanupPair(left, right)
    val testTwoElvenCleanupPair = ElvenCleanupPair(right, left)

    assertEquals(testOneElvenCleanupPair.containsPartiallyOverlappingCleanup, false)
    assertEquals(testTwoElvenCleanupPair.containsPartiallyOverlappingCleanup, false)
  }

  // TODO Write a test of overlaps
}
