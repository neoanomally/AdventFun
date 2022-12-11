package com.sandersme.advent.twentytwo.model

import com.sandersme.advent.twentytwo.model.ElvenCleanupPair.{isFullyOverlappingRange, isPartiallyOverlapplingRange}

case class CleanupRange(start: Int, stop: Int)

object CleanupRange {
  def parseFromString(input: String): CleanupRange = {
    val split = input.split("-")
    assert(split.length == 2)
    CleanupRange(split.head.toInt, split.last.toInt)
  }
}

case class ElvenCleanupPair(leftElf: CleanupRange, rightElf: CleanupRange) {
  def containsFullyOverlappingCleanup: Boolean = {
    isFullyOverlappingRange(leftElf, rightElf) ||
      isFullyOverlappingRange(rightElf, leftElf)
  }

  def containsPartiallyOverlappingCleanup: Boolean = {
    isPartiallyOverlapplingRange(leftElf, rightElf)
  }
}

object ElvenCleanupPair {
  def isPartiallyOverlapplingRange(left: CleanupRange, right: CleanupRange): Boolean = {
    countNumberOverlaps(left, right) > 0
  }

  def countNumberOverlaps(left: CleanupRange, right: CleanupRange): Int = {
    val maxStart = Math.max(left.start, right.start)
    val minStop = Math.min(left.stop, right.stop)

    (minStop - maxStart) + 1
  }

  def isFullyOverlappingRange(left: CleanupRange, right: CleanupRange): Boolean = {
    left.start >= right.start && left.stop <= right.stop
  }

  def parseLine(line: String): ElvenCleanupPair = {
    val split = line.split(",")
    assert(split.length == 2)

    val left = CleanupRange.parseFromString(split.head)
    val right = CleanupRange.parseFromString(split.last)

    ElvenCleanupPair(left, right)
  }

  def fromInput(input: Seq[String]): Seq[ElvenCleanupPair] = {
    input
      .map(parseLine)
  }
}
