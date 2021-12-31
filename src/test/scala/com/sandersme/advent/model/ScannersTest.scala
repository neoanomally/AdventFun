package com.sandersme.advent.model

import com.sandersme.advent.Input
import com.sandersme.advent.model.Scanners.calculateBeaconDistances

class ScannersTest extends munit.FunSuite {
  val LARGER_TEST_SCANNER: Scanners = Scanners.parseIntput(
    Input.readFromDataResource("day19_larger_test"), false)

  test("Validate that I can parse header from --- scanner 0 ---") {
    val headerInput = "--- scanner 0 ---"
    val expectedId = 0
    val parsedA = Scanners.parseHeader(headerInput)

    val header2Input = "--- scanner 25 ---"
    val expected2Id = 25
    val parsedB = Scanners.parseHeader(header2Input)

    assertEquals(parsedA, expectedId)
    assertEquals(parsedB, expected2Id)
  }

  test("Scanner.parseInput will parse a list of scanners based on input") {
    val parsed = LARGER_TEST_SCANNER.allScanners.take(2)

    assertEquals(parsed.size, 2)
    assertEquals(parsed.head.scannerId, 0)
    assertEquals(parsed.last.scannerId, 1)
    assertEquals(parsed.last.beacons.size, 25)
  }

  test("Take a seed beacon and list of beacons and find ones that merge") {
    val beaconA = Beacon(Map.empty, Set(1.0, 2.0, 4.0, 6.0, 8.0))

    val matchingBeacon = Beacon(Map.empty, Set(4.0, 6.0, 8.0))

    val nonMatchingBeacons = Vector(
      Beacon(Map.empty, Set(10.0, 11.0, 12.0)),
      Beacon(Map.empty, Set(13.0, 21.0, 15.0)),
      Beacon(Map.empty, Set(20.0, 19.0, 16.0)),
      Beacon(Map.empty, Set(40.0, 23.0, 17.0))
    )

    val (resultsEmpty, remaining) = MergedBeaconAccum.searchAndMergeBeacon(beaconA, nonMatchingBeacons)

    assertEquals(resultsEmpty, None)
  }

  test("Test mergeBeaconsFromTwoScanners where 12 overlapping Scanners from right side") {
    val leftScanner = LARGER_TEST_SCANNER.allScanners.head
    val rightScanner = LARGER_TEST_SCANNER.allScanners.drop(1).head
    val defaultMergedBeacons = MergedBeacons.empty

    val mergedScanners = Scanners.mergeTwoScannerBeacons(leftScanner.beacons,
      rightScanner.beacons, defaultMergedBeacons)

    val numberMerged = mergedScanners.updatedMergedBeacons.beacons.size

    assertEquals(mergedScanners.updatedLeft.size, leftScanner.beacons.size - numberMerged)
    assertEquals(mergedScanners.updatedRight.size, rightScanner.beacons.size - numberMerged)
    assertEquals(numberMerged, 12)

    val updateScannerZero = LARGER_TEST_SCANNER.updateScanner(0, mergedScanners.updatedLeft)


    val updatedScanners: Scanners =
      LARGER_TEST_SCANNER.updateFromMergedAccum(0, 1, mergedScanners)

    assertEquals(updateScannerZero.allScanners(0).beacons.size, mergedScanners.updatedLeft.size)
    assertEquals(updatedScanners.mergedBeacons.beacons.size, 12)
  }

  test("Find Distinct Beacons looping through all the beacons creating a huge set of mergedBeacons") {
    val firstIterationScanners = Scanners.findDistinctBeacons(LARGER_TEST_SCANNER)
//
//    firstIterationScanners.sharedBeaconsAmongScanners
//      .filter(_.beacons.size >= 12)
//      .filter(shared => shared.scannerA == 0 && shared.scannerB == 1)
//      .foreach{ v =>
//        println(s"${v.scannerA} \t ${v.scannerB}")
//        v.beacons.foreach(pair => println( pair._1 manhattan pair._2))}

    assertEquals(firstIterationScanners.estimatedNumberBeacons, 79)
  }
}
