package com.sandersme.advent.model

import com.sandersme.advent.graph.Coordinate

class MergedBeaconAccumTest extends munit.FunSuite {
  val left: Vector[Beacon] = Vector(
    Beacon(Map(1 -> Coordinate(8, 9, 6)), Set(3, 4, 5, 9))
  )

  val right: Vector[Beacon] = Vector(
    Beacon(Map(0 -> Coordinate(1, 2, 3)), Set(1, 2, 3, 8)),
    Beacon(Map(0 -> Coordinate(2, 3, 4)), Set(3, 4, 5)),
    Beacon(Map(0 -> Coordinate(3, 4, 5)), Set(5, 6, 7))
  )

  val mergedBeacons: MergedBeacons = MergedBeacons.empty

  test("Updating a mergedBeacon when the left finds a beacon based on fingerprint") {
    val startingAccum = MergedBeaconAccum(Vector.empty, right, mergedBeacons)

    val updatedAccum = startingAccum.updateLeftBeacons(left.head)

    assertEquals(startingAccum.updatedLeft.isEmpty, true)
    assertEquals(updatedAccum.updatedLeft.size, 1)
  }

  test("Adding a new beacon to the merged beacons when left is found by looking at items on the right") {
    val startingAccum = MergedBeaconAccum(Vector.empty, right, mergedBeacons)
    val updatedRight = Vector(right.head, right.last)

    val updatedAccum = startingAccum.updateFromRightLookup(left.head, updatedRight)

    assertEquals(updatedAccum.updatedMergedBeacons.beacons.size, 1)
    assertEquals(updatedAccum.updatedRight.size, 2)
  }

  test("Validate updating left only adds the new beacona nd has no additional changes.") {
    val startingAccum = MergedBeaconAccum(Vector.empty, right, mergedBeacons)
    val updatedAccum = startingAccum.updateLeftBeacons(left.head)

    assertEquals(startingAccum.updatedLeft.size, 0)
    assertEquals(updatedAccum.updatedLeft.size, 1)
    assertEquals(updatedAccum.updatedRight, startingAccum.updatedRight)
    assertEquals(updatedAccum.updatedMergedBeacons, startingAccum.updatedMergedBeacons)
  }

  test("Search and Merge Beacon should find one on the fight and update") {
    val startingAccum = MergedBeaconAccum(Vector.empty, right, mergedBeacons)

    val updatedAccum = startingAccum.updateFromSearchAttempt(leftBeacon = left.head)

    assertNotEquals(updatedAccum.updatedRight, startingAccum.updatedRight)
    assertEquals(startingAccum.updatedMergedBeacons.beacons.size, 0)
    assertEquals(updatedAccum.updatedMergedBeacons.beacons.size, 1)
    assertEquals(updatedAccum.updatedLeft.size, 0)
  }
}
