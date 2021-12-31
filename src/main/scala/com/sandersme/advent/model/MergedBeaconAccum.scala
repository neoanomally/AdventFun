package com.sandersme.advent.model

import scala.annotation.tailrec


/**
 * The following case class is a helper class for how we handle the folding left.
 * This method has no knowledge of how anything is being updated to either the left
 * or right. It just has three methods that the caller must know to update the current
 * state of iterating through two scanners:
 *
 * 1.) Updating the mergedBeacons when the left finds a beacon based on fingerprint
 * 2.) Adding a new beacon to the merged beacons when the left is found by looking up items on the right
 * 3.) Accumulating the left side when there are no beacons found either in the merged buckets
 *     OR when the beacon isn't found on the right side.
 *
 * @param updatedLeft
 * @param updatedRight
 * @param updatedMergedBeacons
 */
case class MergedBeaconAccum(updatedLeft: Vector[Beacon],
                             updatedRight: Vector[Beacon],
                             updatedMergedBeacons: MergedBeacons) {


  /**
   *Will update the accumulator based on a beacon that's in the merged beacons
   * it will update the current merged beacons. For optimization we have this
   * updated merged beacons. We don't want to use the original in the edge case
   * where the original is adding values to the merged beacons. This
   * is really optimal for the first iteration where we start out with zero beacons
   */
  def updateFromFingerprint(idx: Int, beacon: Beacon): MergedBeaconAccum = {
    val mergedBeacons = updatedMergedBeacons.updateMergedBeacon(idx, beacon)

    this.copy(updatedMergedBeacons = mergedBeacons)
  }

  // TODO IMPLEMENT ME FUCK BROKE STUFF
  def updateFromFingerprintV2(mergedBeacons: MergedBeacons): MergedBeaconAccum = {
    this.copy(updatedMergedBeacons = mergedBeacons)
  }

  def updateFromSearchAttempt(leftBeacon: Beacon): MergedBeaconAccum = {
    val (maybeMerge: Option[Beacon], right: Vector[Beacon]) =
      MergedBeaconAccum.searchAndMergeBeacon(leftBeacon, updatedRight)

    maybeMerge match {
      case None         => updateLeftBeacons(leftBeacon)
      case Some(beacon) =>
         updateFromRightLookup(beacon, right)
//        updateFromRightLookupV2(beacon, right)
    }
  }

  /**
   * This is the case where we check the right side and update the merged beacons
   * where we iterate through the right. We need to reconstruct the right side minus
   * the beacon we found.
   *
   * @param newMergedBeacon
   * @param right
   * @return
   */
  private[model]  def updateFromRightLookup(newMergedBeacon: Beacon,
                                            right: Vector[Beacon]): MergedBeaconAccum = {
    val mergedBeacons = updatedMergedBeacons.addNewBeacon(newMergedBeacon)

    this.copy(updatedMergedBeacons = mergedBeacons, updatedRight = right)
  }

  private[model]  def updateFromRightLookupV2(newMergedBeacon: Beacon,
                                              right: Vector[Beacon]): MergedBeaconAccum = {
    val mergedBeacons = updatedMergedBeacons.addNewBeaconV2(newMergedBeacon)

    this.copy(updatedMergedBeacons = mergedBeacons, updatedRight = right)
  }


  /**
   * As we iterate through the left scanner we'll add a beacon one by one from the original
   * left to this one. This is optimized so that we aren't removing left side as we find them
   * instead we add left side as we don't find them on the right or in the merged tables
   * @param beacon
   * @return
   */
  private[model] def updateLeftBeacons(beacon: Beacon): MergedBeaconAccum = {
    val leftBeacons = updatedLeft :+ beacon

    this.copy(updatedLeft = leftBeacons)
  }

}

object MergedBeaconAccum {
  /**
   * This method is optimized to end early. AS SOON as we find the first matching beacon
   * we exit out of the loop. Worst case scenario there are no matches and we reach the end
   * of the list.
   *
   * @param seed: The seed beacon we are trying to match on
   * @param beacons
   * @param remainingBeacons
   * @return The
   */
  @tailrec
  private[model] def searchAndMergeBeacon(seed: Beacon, beacons: Vector[Beacon],
                                          remainingBeacons: Vector[Beacon] = Vector.empty):
  (Option[Beacon], Vector[Beacon]) = {

    beacons match {
      case head +: tail =>
        if (Beacon.shouldMergeBeacons(seed,beacons.head))
          val mergedBeacon = Beacon.mergeBeacons(seed, beacons.head)
          val updatedRemaining = remainingBeacons ++ tail

          (Some(mergedBeacon), updatedRemaining)
        else
          searchAndMergeBeacon(seed, tail, remainingBeacons :+ head)
      case _ => (None, remainingBeacons)
    }
  }



}