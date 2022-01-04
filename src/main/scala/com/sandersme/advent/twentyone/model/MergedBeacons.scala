package com.sandersme.advent.twentyone.model

import scala.annotation.tailrec
import scala.collection.mutable


// TODO Put the fingerprints in a bloom filter.
type FingerPrintsMap = mutable.Map[Set[Double], Int]
type FingerPrints = Set[Set[Double]]

case class MergedBeacons(beacons: Vector[Beacon] = Vector.empty,
                         fingerPrintsMap: FingerPrintsMap = mutable.Map.empty[Set[Double], Int],
                         flatFingerPrints: Set[Double] = Set.empty) {
  def findFingerPrint(beacon: Beacon): Option[Int] = {
    MergedBeacons.findFingerPrint(beacon.fingerPrints, this)
  }

  def findFingerPrintV2(beacon: Beacon): Option[MergedBeacons] = {
    updateMergedIndexV2(beacon)
  }

  @tailrec
  final private[model] def updateMergedIndexV2(beacon: Beacon, beaconIdx: Int = 0): Option[MergedBeacons] = {
    if (beaconIdx >= beacons.size) {
      None
    } else if  (fastSetExists(beacon.neighbors, beacons(beaconIdx).neighbors)) {
      val updatedBeacon = Beacon.mergeBeaconV2(beacon, beacons(beaconIdx))
      val updatedBeacons = beacons.updated(beaconIdx, updatedBeacon)
      val addedNeighbors = flatFingerPrints ++ beacon.neighbors
      Some(MergedBeacons(beacons = updatedBeacons, flatFingerPrints = addedNeighbors))
    } else {
      updateMergedIndexV2(beacon, beaconIdx + 1)
    }
  }

  /**
   * This is an optimized function that if there are overlapping elements from left in the right then
   * it will exit early. The more items we have in here the mfaster it should be.
   */
  @tailrec
  final private[model] def fastSetExists(left: Set[Double], right: Set[Double],
                                         matches: Int = 0): Boolean = {
    if(matches >= 2)
      true
    else if (left.isEmpty)
      false
    else
      val updatedMatches =  if (right.contains(left.head)) matches + 1 else matches
      fastSetExists(left.tail, right, updatedMatches)
  }

  /** For a given index merge the beacon that needs to be updated
   * into the current mergedBeacons. */
  def updateMergedBeacon(idx: Int, beacon: Beacon): MergedBeacons = {
    val currentBeacon = beacons(idx)
    val updatedBeacon = Beacon.mergeBeacons(beacon, currentBeacon)
    val updatedBeacons = beacons.updated(idx, updatedBeacon)
    val printsToAdd = beacon.fingerPrints.map(_ -> idx)
    val updatedFingerPrintMap = fingerPrintsMap ++ printsToAdd

    MergedBeacons(updatedBeacons, updatedFingerPrintMap)
  }

  def addNewBeacon(beacon: Beacon): MergedBeacons = {
    val newIndex = beacons.size

    val fingerPrints = beacon.fingerPrints.map(print => print -> newIndex)
    val updatedBeacons = beacons :+ beacon
    val updatedFingerPrints = fingerPrintsMap ++ fingerPrints

    MergedBeacons(updatedBeacons, updatedFingerPrints)
  }

  def addNewBeaconV2(beacon: Beacon): MergedBeacons = {
    val idx = beacons.size

    val updatedBeacons = beacons :+ beacon
    val updatedFingerPrints = flatFingerPrints ++ beacon.neighbors

    MergedBeacons(beacons = updatedBeacons, flatFingerPrints = updatedFingerPrints)
  }
}

object MergedBeacons {
  def empty: MergedBeacons = MergedBeacons()

  /**
   * Using tail recursion for quick exit. If we find the fingerprint
   * in the emergedBeaconsFingerprint map we exit out early.
   *
   * @param fingerPrints
   * @param mergedBeacons
   * @return
   */
  @tailrec
  private[model] def findFingerPrint(fingerPrints: FingerPrints,
                                     mergedBeacons: MergedBeacons): Option[Int] = {

    fingerPrints.headOption match {
      case None => None
      case Some(head) =>
        val beaconLocation = mergedBeacons.fingerPrintsMap.get(head)
        if (beaconLocation.isDefined)
          beaconLocation
        else
          findFingerPrint(fingerPrints.tail, mergedBeacons)
    }
  }
}