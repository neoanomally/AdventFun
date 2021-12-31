package com.sandersme.advent.model

import com.sandersme.advent.graph.Coordinate

class BeaconTest extends munit.FunSuite {
  test("parseInput from line") {
    val input = List("-2,-2,2", "-3,-3,3", "-2,-3,1", "5,6,-4")

    val beacons = input.map(v => Beacon.parseInput(v, 1))

    assertEquals(beacons.size, 4)
    assertEquals(beacons.last.scannerLocs.head._2, Coordinate(5, 6, -4))
  }

  test("Check on whether or not two beacons should merge") {
    val beaconA = Beacon(Map.empty, Set(1.0, 3.0, 5.0, 7.0, 8.0))
    val beaconB = Beacon(Map.empty, Set(5.0, 7.0, 8.0))
    val beaconC = Beacon(Map.empty, Set(2.0, 3.0, 4.0, 9.0))

    assert(!Beacon.shouldMergeBeacons(beaconB, beaconC))
    assert(!Beacon.shouldMergeBeacons(beaconA, beaconC))
    assert(Beacon.shouldMergeBeacons(beaconA, beaconB))
  }

  test("Merge beacons should create a merged beacon with the set of all neighbors") {
    val beaconA = Beacon(Map.empty, Set(1.0, 3.0, 5.0, 7.0, 8.0))
    val beaconB = Beacon(Map.empty, Set(5.0, 7.0, 8.0))

    val mergedBeacon = Beacon.mergeBeacons(beaconA, beaconB)
    val expectedBeacon = Beacon(Map.empty, Set(1.0, 3.0, 5.0, 7.0, 8.0))
    assertEquals(mergedBeacon, expectedBeacon)
  }
}
