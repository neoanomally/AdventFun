package com.sandersme.advent.model

import com.sandersme.advent.model.BinaryCoding
import com.sandersme.advent.model.BinaryCoding._
import com.sandersme.advent.model.BitTypeCounter

class Diagnostics(binaryCodings: List[BinaryCoding]){
  // Calculates the binary length of the BinaryCoding
  val binaryLength: Int = binaryCodings.headOption.map(_.bits.size).getOrElse(0)

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

  /** To find oxygen generator rating, determine the most common value (0 or 1) in the current
   * bit position, and keep only numbers with that bit in that position. If 0 and 1 are equally common,
   * keep values with a 1 in the position being considered.
   */
  def oxygenGeneratorRating: Int = {
      val bitToKeepFunction: (Int, Int) => Bit =
        (zeroCount, oneCount) => if (oneCount >= zeroCount) One else Zero

      val results = eliminateBinaryCodingRows(binaryCodings, bitToKeepFunction)
      results
        .map(_.toInt)
        .get
    }

  /**
   * This uses the least common bit in each column.
   * Then filters out all the rows that doesn't have the least common bit
   */
  def co2ScrubberRating: Int = {
    val bitToKeepFunction: (Int, Int) => Bit = (zeroCount, oneCount) =>
      if (zeroCount <= oneCount) Zero else One

    val results = eliminateBinaryCodingRows(binaryCodings, bitToKeepFunction)
    results
      .map(_.toInt)
      .get
  }

  def lifeSupportRating: Int = co2ScrubberRating * oxygenGeneratorRating

  // TODO Figure out what to return, right now it's an Option of Binary Coding
  // just in case we have an empty. The above methods do a get just to fail anyways.
  // I can imagine some logic where we take the head/tail of the previous filtered out set.

  /**
   * This is a very interesting function. What we are doing is going through each column
   * and eliminating either the least common or most common bit in that column.
   * The least column and most common bits are calculated by using a sub Diagnostics
   * class that calls the totalBitCounters method which sums up the zeroes and ones for each
   * column. Those BitTypeCounters are used to evaluate the least or most common bit for those
   * columns. CO2 Scrubbing Rating uses Zero as priority and the least common bit and
   * Oxygen generator uses most common bit and Ones as priority. So we take in a function instead
   * of using multiple if statements and some flag.
   *
   * Once we know the bitToKeep we then go through each row and filter when the current column
   * we are on matches the most common bit. We keep doing this until there is ONE BinaryCode
   * left.
   *
   * @param codingsLeft: The set of BinaryCoding List of the valid binary codings left
   * @param bitToKeepFun: Function that figures out the most significant or least signifant bit
   * @param columnPosition: Current column we are removing BinaryCodings for
   * @return: Option of the Binary Coding based on scanning through our list of binary codings
   */
  private def eliminateBinaryCodingRows(codingsLeft: List[BinaryCoding], bitToKeepFun: (Int, Int) => Bit,
                                columnPosition: Int = 0): Option[BinaryCoding] = {
    val currentTotalBitCounters = Diagnostics(codingsLeft).totalBitCounters

    val zeroCount = currentTotalBitCounters(columnPosition).zeroCount
    val oneCount = currentTotalBitCounters(columnPosition).oneCount

    val bitToKeep: Bit = bitToKeepFun(zeroCount, oneCount)

    val updatedCodings: List[BinaryCoding] = codingsLeft
      .filter(_.bits(columnPosition) == bitToKeep)

    // One way to handle a situation where we are on the final column and haven't found
    // one binary diagnostic because everything got eliminated or the final set has too
    // many items is to take the head or tail... This might be a nonissue for this test.
    if (updatedCodings.size <= 1 || columnPosition + 1 >= binaryLength) {
      updatedCodings.headOption
    } else {
      eliminateBinaryCodingRows(updatedCodings, bitToKeepFun, columnPosition + 1)
    }
  }
}

object Diagnostics {
  /**
   * Map the bitCodings into BitCounters
   * Combine two rows of BitCounters
   * Then add each of the columns together.
   *
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