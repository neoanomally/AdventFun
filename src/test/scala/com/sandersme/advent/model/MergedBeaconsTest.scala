package com.sandersme.advent.model

import com.sandersme.advent.graph.Coordinate
import sbt.testing.Fingerprint

class MergedBeaconsTest extends munit.FunSuite {

  test("Update a merged beacon based on index") {
    val emptyMergedBeacon = MergedBeacons.empty
    val beaconA = Beacon(Map(0 -> Coordinate(1, 2, 3)), Set(1, 2, 3, 4))

    val beaconB = Beacon(Map(1 -> Coordinate(4, 5, 6)), Set(2, 3, 4, 5))

    val updatedMergedBeacon = emptyMergedBeacon.addNewBeacon(beaconA)
    val updatedMergedAWithB = updatedMergedBeacon.updateMergedBeacon(0, beaconB)
    val mergedBeacon = updatedMergedAWithB.beacons.head

    val expectedMergedBeacon = Beacon(beaconB.scannerLocs ++ beaconA.scannerLocs,
      Set(1, 2, 3, 4, 5))

    assertEquals(mergedBeacon, expectedMergedBeacon)
  }

  test("Add a new merged beacon, validate that it has the correct index after inserting") {
    val emptyMergedBeacon = MergedBeacons.empty
    val beaconA = Beacon(Map(0 -> Coordinate(1, 2, 3)), Set(1, 2, 3, 4))
    val beaconB = Beacon(Map(1 -> Coordinate(4, 5, 6)), Set(2, 3, 4, 5))
    val beaconC = Beacon(Map(2-> Coordinate(8, 9, 7)), Set(6, 7, 8))

    val updatedMergedBeacon = emptyMergedBeacon.addNewBeacon(beaconA)
    val shouldMergeShouldBeTrue = updatedMergedBeacon.findFingerPrint(beaconB).isDefined
    val shouldMergeShouldBeFalse = updatedMergedBeacon.findFingerPrint(beaconC).isDefined

    assertEquals(emptyMergedBeacon.beacons.isEmpty, true)
    assertEquals(emptyMergedBeacon.fingerPrintsMap.isEmpty, true)
    assertEquals(updatedMergedBeacon.beacons.size,  1)
    assertEquals(updatedMergedBeacon.findFingerPrint(beaconA),  Some(0))
    assertEquals(shouldMergeShouldBeTrue, true)
    assertEquals(shouldMergeShouldBeFalse, false)
  }
}
