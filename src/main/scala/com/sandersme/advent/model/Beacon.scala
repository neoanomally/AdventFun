package com.sandersme.advent.model

import com.sandersme.advent.graph.Coordinate
import com.sandersme.advent.math.VectorDistanceSyntax.*
import com.sandersme.advent.model.LavaFlow.ValueNeighbors

/**
 * TODO: Each Beacon will contain a MAP with Scanner to
 * coordingates. This Map will give the beacons position
 * relative to that scanner.
 * ONce we merge all beacons WE SHOULD BE ABLE
 */
case class Beacon(scannerLocs: Map[Int, Coordinate],
                  neighbors: Set[Double],
                  fingerPrints: Set[Set[Double]]) {
  override def toString: String = s"Beacon(Neighbors: $neighbors) \t $scannerLocs"

  def asVector(id: Int): Vector[Int] = scannerLocs(id).asVector
}

object Beacon {

  def apply(scannerLocs: Map[Int, Coordinate],
            neighbors: Set[Double]): Beacon = {

    val fingerPrints: Set[Set[Double]] = neighbors
      .toList
      .combinations(3)
      .map(_.toSet)
      .toSet

    Beacon(scannerLocs, neighbors, fingerPrints)
  }

  def apply(scannerLoc: Map[Int, Coordinate]): Beacon = {
    Beacon(scannerLoc, Set.empty, Set.empty)
  }

  def parseInput(input: String, scannerId: Int): Beacon = {
    val split: Array[String] = input.split(",")
    require(split.length == 3, "Fatal Error. Beacons require three dimensions")
    val (x: Int, y: Int, z: Int) = (split(0).toInt, split(1).toInt, split(2).toInt)
    Beacon(Map(scannerId -> Coordinate(x, y, z)))
  }

  /**
   * The nearest neighbors are euclidean distance between the beacon and
   * all it's nearest neighbors. Because the neighbors are Double
   * the combination of nearest neighbors create a unique footprint due
   * to the precision any overlap of 3 neighbors should indicate that the beacon
   * is the same. From the Scanner class in this instance we are keeping the nearest
   * 6 neighbors. I do this in case some of those are on the boundary. THe problem
   * that can occur when keeping the nearest neighbors is if those neighbors are
   * on the other side of the boarder. One of the things I could do to modify this
   * is take 3 of the nearest neighbors that are closer to the scanner AND the three
   * nearestneighbors that are further from the scanner. :Shrug:
   * @param left
   * @param right
   * @return
   */
  def shouldMergeBeacons(left: Beacon, right: Beacon): Boolean = {
    left.neighbors.intersect(right.neighbors).size >= 3
  }

  /** Given two beacons create one with the combinations of their neighbors
   * and updating the location to both scannerLocs. This MAY be useful later
   * @param left
   * @param right
   * @return
   */
  def mergeBeacons(left: Beacon, right: Beacon): Beacon = {
    val updatedScannerLocs = left.scannerLocs ++ right.scannerLocs
    val updatedNeighbors = left.neighbors ++ right.neighbors

    Beacon(updatedScannerLocs, updatedNeighbors)
  }

  def mergeBeaconV2(left: Beacon, right: Beacon): Beacon = {
    val updatedScannerLocs = left.scannerLocs ++ right.scannerLocs
    val updatedNeighbors = left.neighbors ++ right.neighbors

    Beacon(updatedScannerLocs, updatedNeighbors, Set.empty)
  }

}
