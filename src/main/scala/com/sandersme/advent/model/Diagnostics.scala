package com.sandersme.advent.model

import com.sandersme.advent.model.BinaryCoding
import com.sandersme.advent.model.BinaryCoding._
import com.sandersme.advent.model.BitTypeCounter

class Diagnostics(binaryCodings: List[BinaryCoding]){
  lazy val totalBitCounters: List[BitTypeCounter] = sumBinaryCodingColumns

  private def sumBinaryCodingColumns: List[BitTypeCounter] = {
    val counters = binaryCodings
      .map(_.toBitCounters)
      .reduce { case (leftCounterList, rightCounterList) =>
        val zippedCounterList = leftCounterList zip rightCounterList
        zippedCounterList
          .map { case (leftBitCounter, rightBitCounter) =>
            leftBitCounter += rightBitCounter
          }
      }

    counters
  }


  /**
   * Each bit in the gamma rate can be determined by finding the most common bit in the corresponding
   * position of all numbers in the diagnostic report.
   * Each bit in the episolon rate can be determined by finding the least common bit in the corresponding
   * position of all numbers in the diagnostic report.
   */
  def gammaRate: BinaryCoding = {
    val bits = totalBitCounters.map { bitTypeCounter =>
      if (bitTypeCounter.oneCount >= bitTypeCounter.zeroCount) {
        One
      } else {
        Zero
      }
    }

    BinaryCoding(bits)
  }

  def epsilonRate: BinaryCoding = {
    val bits = gammaRate.bits.map {
      case One => Zero
      case Zero => One
    }

    BinaryCoding(bits)
  }

  def powerConsumption: Int = {
    epsilonRate *= gammaRate
  }
}

object Diagnostics {
  /**
   * Map the bitCodings into BitCounters
   * Combine two rows of BitCounters
   * Then add each of the columns together.
   *
   * TODO: We should have a way to parse input: String and input: List[String]
   * to create diagnostics from
   * class to indicate that the int are variable fields.
   */
  def fromBinaryInput(binaryInput: List[String]): Diagnostics = binaryInput
    .map(BinaryCoding.apply)
    .toDiagnostics

  extension (binaryCodingList: List[BinaryCoding]) {
    def toDiagnostics: Diagnostics = Diagnostics(binaryCodingList)
  }
}