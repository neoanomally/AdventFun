package com.sandersme.advent.model

import BinaryCoding._

case class OxygenGenerator(rating: Int)
case class C02Scrubber(rating: Int)


case class BinaryCoding(bits: List[Bit]) {
  override def toString: String = {
    bits.map(bit => bit match {
      case Zero => "0"
      case One => "1"
    }).reduce(_ + _)
  }

  def toInt: Int = {
    Integer.parseInt(toString, 2)
  }
}

object BinaryCoding {
  sealed trait Bit
  object One extends Bit
  object Zero extends Bit


  def apply(input: String): BinaryCoding = {
    val bits = input.toList
      .map(value => if(value == '1') One else Zero)

    BinaryCoding(bits)
  }

  extension (binaryCoding: BinaryCoding) {
    def toBitCounters: BitTypeCounters = {
      val counters: List[BitTypeCounter] = binaryCoding
        .bits
        .map(bit => bit match {
          case Zero => BitTypeCounter(1, 0)
          case One => BitTypeCounter(0, 1)
        })

      BitTypeCounters(counters)
    }

    def *=(other: BinaryCoding): Int = {
      binaryCoding.toInt * other.toInt
    }
  }

  /**
   * Map the bitCodings into BitCounters
   * Combine two rows of BitCounters
   * Then add each of the columns together.
   *
   * TODO Instead of returning a List of BitTypeCounter we should have a container
   * class to indicate that the int are variable fields.
   */
  extension (binaryCodingList: List[BinaryCoding])
    def sumBinaryCodingColumns: BitTypeCounters = {
      val counters = binaryCodingList
        .map(_.toBitCounters.counters)
        .reduce{ case (leftCounterList, rightCounterList) =>
          val zippedCounterList = leftCounterList zip rightCounterList
          zippedCounterList
            .map{ case(leftBitCounter, rightBitCounter) =>
              leftBitCounter += rightBitCounter
            }
        }

      BitTypeCounters(counters)
    }
}

/**
 * Each bit in the gamma rate can be determined by finding the most common bit in the corresponding
 * position of all numbers in the diagnostic report.
 * Each bit in the episolon rate can be determined by finding the least common bit in the corresponding
 * position of all numbers in the diagnostic report.
 *
 * // TODO FIGURE OUT WHAT TO DO WHEN THEY ARE EQUAL. FOR NOW WE'll have them members of Both
 * // TODO: Move these things into their own files
 */
case class BitTypeCounters(counters: List[BitTypeCounter]) {
  def gammaRate: BinaryCoding = {
    val bits = counters.map { bitTypeCounter =>
      if (bitTypeCounter.oneCount >= bitTypeCounter.zeroCount) {
        One
      } else {
        Zero
      }
    }

    BinaryCoding(bits)
  }

  def epsilonRate: BinaryCoding = {
    val bits = gammaRate.bits.map{ bit =>
      bit match {
        case One => Zero
        case Zero => One
      }
    }

    BinaryCoding(bits)
  }

  def powerConsumption: Int = {
    epsilonRate *= gammaRate
  }
}
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