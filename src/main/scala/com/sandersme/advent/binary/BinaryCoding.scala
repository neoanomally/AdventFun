package com.sandersme.advent.binary

import com.sandersme.advent.binary.{Bit, Bits}

case class BinaryCoding(bits: List[Bit]) {
  override def toString: String = {
    bits.map {
      case Zero => "0"
      case One => "1"
    }.reduce(_ + _)
  }

  def toInt: Int = {
    Integer.parseInt(toString, 2)
  }
}

object BinaryCoding {
  def apply(input: String): BinaryCoding = {
    val bits = Bits.parseBinary(input)
    BinaryCoding(bits.toList)
  }

  extension (binaryCoding: BinaryCoding) {
    def toBitCounters: List[BitTypeCounter] = {
      val counters: List[BitTypeCounter] = binaryCoding
        .bits
        .map {
          case Zero => BitTypeCounter(1, 0)
          case One => BitTypeCounter(0, 1)
        }

      counters
    }

    def *=(other: BinaryCoding): Int = {
      binaryCoding.toInt * other.toInt
    }
  }
}