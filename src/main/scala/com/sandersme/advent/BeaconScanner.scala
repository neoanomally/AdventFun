package com.sandersme.advent

import com.sandersme.advent.model.Scanners

object BeaconScanner {
  /**
   * * As your probe drifted down through this area, it released an assortment of
   * beacons and scanners into the water. It's difficult to navigate in the pitch
   * black open waters of the ocean trench, but if you can build a map of the trench
   * using data from the scanners, you should be able to safely reach the bottom.
   *
   * The beacons and scanners float motionless in the water; they're designed to
   * maintain the same position for long periods of time. Each scanner is capable of
   * detecting all beacons in a large cube centered on the scanner; beacons that are
   * at most 1000 units away from the scanner in each of the three axes (x, y, and
   * z) have their precise position determined relative to the scanner. However,
   * scanners cannot detect other scanners. The submarine has automatically
   * summarized the relative positions of beacons detected by each scanner (your
   * puzzle input).
   *
   * For example, if a scanner is at x,y,z coordinates 500,0,-500 and there are
   * beacons at -500,1000,-1500 and 1501,0,-500, the scanner could report that the
   * first beacon is at -1000,1000,-1000 (relative to the scanner) but would not
   * detect the second beacon at all.
   *
   * Unfortunately, while each scanner can report the positions of all detected
   * beacons relative to itself, the scanners do not know their own position. You'll
   * need to determine the positions of the beacons and scanners yourself.
   *
   * The scanners and beacons map a single contiguous 3d region. This region can be
   * reconstructed by finding pairs of scanners that have overlapping detection
   * regions such that there are at least 12 beacons that both scanners detect
   * within the overlap. By establishing 12 common beacons, you can precisely
   * determine where the scanners are relative to each other, allowing you to
   * reconstruct the beacon map one scanner at a time.
   *
   * Unfortunately, there's a second problem: the scanners also don't know their
   * rotation or facing direction. Due to magnetic alignment, each scanner is
   * rotated some integer number of 90-degree turns around all of the x, y, and z
   * axes. That is, one scanner might call a direction positive x, while another
   * scanner might call that direction negative y. Or, two scanners might agree on
   * which direction is positive x, but one scanner might be upside-down from the
   * perspective of the other scanner. In total, each scanner could be in any of 24
   * different orientations: facing positive or negative x, y, or z, and considering
   * any of four directions "up" from that facing.
   *
   * In total, there are 79 beacons.
   *
   * Assemble the full map of beacons. How many beacons are there?
   * TODO Implement the memory efficient version
   *
   * @param args
   */
  def main(args: Array[String]): Unit = {
    val input = Input.readFromDataResource("day19_input")

    val inputScanners = Scanners.parseIntput(input, true)
    val scannersWithDistinctBeacons = Scanners.findDistinctBeacons(inputScanners)
    val distinctNumberBeacons = scannersWithDistinctBeacons.estimatedNumberBeacons

    println(s"The number of distinct beacons: $distinctNumberBeacons")

    println(s"MergedBeacons: ${scannersWithDistinctBeacons.mergedBeacons.beacons.size}")
  }
}
