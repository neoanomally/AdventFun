package com.sandersme.advent.twentyone.binary

/**
 * The BitTypeCounter is a counter that's useful for claculating diagnostics report. They are good
 * at summing up multiple binaries related to diagnostics reports.
 *
 * @param zeroCount
 * @param oneCount
 */
case class BitTypeCounter(zeroCount: Int, oneCount: Int) {
  def += (thatBitCounter: BitTypeCounter): BitTypeCounter = {
    val zeroes = zeroCount + thatBitCounter.zeroCount
    val ones = oneCount + thatBitCounter.oneCount

    BitTypeCounter(zeroes, ones)
  }
}

object BitTypeCounter {
  val DEFAULT_BIT_COUNTER = BitTypeCounter(0, 0)
}

