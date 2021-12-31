package com.sandersme.advent.model

import com.sandersme.advent.math.VectorDistanceSyntax.*

import java.security.MessageDigest
import scala.annotation.tailrec
import scala.collection.immutable

/**
 * The Scanners class holds all the scanners and merged beacons. As the classes
 * attempt to merge beacons the number of beacons per scanner should disappear and
 * the beacons will go to the mergedBeacons
 *
 * @param allScanners
 * @param allDistances is really to optimize for the tests / smaller sets. We don't need
 *                     All the distances for the tests due to the space size. This is to prevent
 *                     the space sizes from exploding
 * @param mergedBeacons
 */
case class Scanners(allScanners: Vector[Scanner], mergedBeacons: MergedBeacons, allDistances: Boolean = false) {
  def updateMergedBeacons(updatedMergedBeacons: MergedBeacons): Scanners = {
    this.copy(mergedBeacons = updatedMergedBeacons)
  }

  def distanceBetweenAllScanners: Map[(Int, Int), Long] = {
    val scannerDistanceMap = defaultScannerDistances

    val allDistances: Seq[Iterable[((Int, Int), Long)]] =
      mergedBeacons
        .beacons
        .map(_.scannerLocs)
        .flatMap { scannerLoc =>
          scannerLoc.keys.flatMap { leftKey =>
            scannerLoc.keys.map { rightKey =>
              val leftLoc = scannerLoc(leftKey)
              val rightLoc = scannerLoc(rightKey)
              val min = Math.min(leftKey, rightKey)
              val max = Math.min(leftKey, rightKey)
              val distance = leftLoc manhattan rightLoc
              println(s"$leftKey $rightKey $distance")

              (min, max) -> distance
            }.filter(pair => pair._1._1 != pair._1._2)
          }
          ???
        }

//    allDistances.toMap
    ???
  }

  private[model] def defaultScannerDistances: Map[(Int, Int), Option[Long]] = {
    val scannerCombinations: Seq[(Int, Int)] = for {
      i <- allScanners.indices
      j <- allScanners.indices

      min = Math.min(i, j)
      max = Math.max(i, j)

      if (i != j)
    } yield (min, max)

    scannerCombinations.map(i => i -> None).toMap
  }

  /**
   * This is a convenience method for updating the Scanners Beacons and updating
   * the specific Scanner with the Beacon.
   * @param scanIdx
   * @param updatedBeacons
   * @return
   */
  def updateScanner(scanIdx: Int, updatedBeacons: Vector[Beacon]): Scanners = {

    val scanner = allScanners(scanIdx)
    val updatedScanner = scanner.copy(beacons = updatedBeacons)
    val updatedScanners = allScanners.updated(scanIdx, updatedScanner)

    this.copy(allScanners = updatedScanners)
  }

  /**
   * Take a MergedBeaconsAccumulator and update the mergedBeacons,
   * the beacons from leftIdx Scanner and the beacons for the rightIdx Scanner
   *
   * @return Copy of this Scanners to a new Scanners
   */
  private[model] def updateFromMergedAccum(leftIdx: Int, rightIdx: Int,
                                           mergedBeacons: MergedBeaconAccum): Scanners = {
    this.updateMergedBeacons(mergedBeacons.updatedMergedBeacons)
      .updateScanner(leftIdx, mergedBeacons.updatedLeft)
      .updateScanner(rightIdx, mergedBeacons.updatedRight)
  }

  /**
   * After all overlapping beacons have been merged we need to add those
   * to the number of beacons that had no mergings. This should give us the approximate
   * number of distinct beacons
   * @return
   */
  def estimatedNumberBeacons: Int = {
    mergedBeacons.beacons.size + allScanners.map(_.beacons.size).sum
  }
}

object Scanners {
  def parseIntput(input: List[String], allDistances: Boolean = true): Scanners = {
    val defaultAccumulator: (Vector[Scanner], Option[Scanner]) = (Vector.empty[Scanner], None)

      val (accumu, scanner) = input.foldLeft(defaultAccumulator) {
        case ((scannAccum, scanner), line) =>
          if (line.isEmpty) {
            (scannAccum :+ scanner.get, None)
          } else if (line.contains("scanner")){
            val scannerId = parseHeader(line)
            (scannAccum, Some(Scanner(scannerId, Vector.empty)))
          } else {
            val scannerId = scanner.get.scannerId
            val newBeacon = Beacon.parseInput(line, scannerId)
            val updatedScanner = scanner.map(scan => scan.copy(beacons = scan.beacons :+ newBeacon))
            (scannAccum, updatedScanner)
          }
      }
    val finalAccum = accumu :+ scanner.get
    val scannersWithDistances = addDistancesToAllBeacons(finalAccum, allDistances)

    Scanners(scannersWithDistances, MergedBeacons.empty)
  }

  def addDistancesToAllBeacons(scanners: Vector[Scanner], allDistances: Boolean = true): Vector[Scanner] = {
    scanners.map(scan => calculateBeaconDistances(scan, allDistances))
  }

  // TODO: I'm dumb I thought of a simpler way to do this instead of doing
  // inner foldLefts. I can instead take the first scanner and add all those to the
  // merged beacons, then go through each scanner twice building
  @tailrec
  def findDistinctBeacons(scanners: Scanners, leftIdx: Int = 0): Scanners = {
    if (leftIdx >= scanners.allScanners.size)
      scanners
    else
      val updatedScanner = findDistinctInner(scanners, leftIdx, leftIdx + 1)

      findDistinctBeacons(updatedScanner, leftIdx + 1)
  }

  /**
   * This is a helper method so that I don't have inner foldLeft / inner Recursion.
   * for a given Scanner leftIndex, go through all the scanners on the right side.
   *
   */
  @tailrec
  private[model] def findDistinctInner(scanners: Scanners, leftIdx: Int,
                                       rightIdx: Int = 0) : Scanners = {
      if (rightIdx >= scanners.allScanners.size) {
        scanners
      } else if (leftIdx == rightIdx) {
        findDistinctInner(scanners, leftIdx, rightIdx + 1)
      } else {
        val leftBeacons = scanners.allScanners(leftIdx).beacons
        val rightBeacons = scanners.allScanners(rightIdx).beacons

        val mergedBeaconsAccum =
          mergeTwoScannerBeacons(leftBeacons, rightBeacons, scanners.mergedBeacons)

        val updatedScanners = scanners.updateFromMergedAccum(leftIdx, rightIdx, mergedBeaconsAccum)
        findDistinctInner(updatedScanners, leftIdx, rightIdx + 1)
      }
    }


  /**
   * Calculates the euclidean distance between all the beacons with relation
   * to one another. At this point I know I need to orient the scanners, but
   * maybe I can figure out scanners with overlapping beacons first using
   * vector math. Then figure out the angles between the scanner and the items
   * to figure out the best way to orient the scanners? Just a thought right now may
   * change my mind.
   *
   * This operation is O(Pow(B, 2)) * S
   *
   * @param scanner
   * @return
   */
  private[model] def calculateBeaconDistances(scanner: Scanner, allDistances: Boolean = true): Scanner = {
    val beacons = scanner.beacons
    val updatedBeacons = beacons.map{ beacon =>
      val allNeighbors = beacons.filter(_ != beacon).map{ neighbor =>
        beacon.asVector(scanner.scannerId) euclidean neighbor.asVector(scanner.scannerId)
      }.sorted

      val partialNeighbors = if(allDistances) allNeighbors else allNeighbors.take(8)

      Beacon(beacon.scannerLocs,neighbors = partialNeighbors.toSet)
    }

    scanner.copy(beacons = updatedBeacons)
  }


  /**
   * 1. Loop through every element on the left.
   * 2. Check to see if the left Element has already been found in the mergedBeacons
   * 3. Find an element on the right.
   *    3a. 1. If there is a match on the right merge them together
   *        2. Remove the element on the right from the list.
   *        3. Don't add the current left element back to the list
   *        4. The merged element gets added to the newly merged items.
   *    3b. 1. If there are no merges the right doesn't need updated.
   *        2. The left needs to add it's element to the updatedLeft.
   * 4. These updated need to add to the accumulator and update the MergedBeaconResults
   *
   * Some other process can handle how we take the newlyMergedBeacons, the mergedBeacons
   * updatedLeft and updatedRight.
   *
   * @param left These Beacons should all belong to scanner A
   * @param right These Beacons should all belong to scanner B
   * @param newlyMergedBeacons. New beacons that have been merged
   * @return MergedBeaconAccumulator: This will accumulate the latest State at each
   *         iteration. This will add Left Values that weren't found in the right
   *         And will remove values from the right as we find values. Any
   *         new mergedbeacons will be added to the accumulator.
   */
  private[model] def mergeTwoScannerBeacons(left: Vector[Beacon], right: Vector[Beacon],
                                            mergedBeacons: MergedBeacons): MergedBeaconAccum = {
    val defaultAccum = MergedBeaconAccum(Vector.empty, right, mergedBeacons)

    left.foldLeft(defaultAccum) { case (mergedBeaconAccum, leftBeacon) =>
//      val findFromMergedBeacons: Option[MergedBeacons] = mergedBeacons.findFingerPrintV2(leftBeacon)
      val findFromMergedBeacons: Option[Int] = mergedBeacons.findFingerPrint(leftBeacon)
      if (findFromMergedBeacons.isDefined) {
                val idx = findFromMergedBeacons.get
                mergedBeaconAccum.updateFromFingerprint(idx, leftBeacon)
//        mergedBeaconAccum.updateFromFingerprintV2(findFromMergedBeacons.get)
      } else {
        mergedBeaconAccum.updateFromSearchAttempt(leftBeacon)
      }
    }
  }

  /**
   * The header should look something like --- scanner 0 ---
   * @param header
   * @return
   */
  private[model] def parseHeader(header: String): Int = {
    if (header.contains("scanner"))
      header
        .split("scanner ")(1).dropRight(4).toInt
    else
      throw new Exception(s"Fatal Error processing header: $header does not contain scanner")
  }
}
