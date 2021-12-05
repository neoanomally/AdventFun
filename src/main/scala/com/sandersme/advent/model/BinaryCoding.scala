package com.sandersme.advent.model

import BinaryCoding._
import com.sandersme.advent.model.BitTypeCounter

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
  sealed trait Bit
  object One extends Bit
  object Zero extends Bit


  def apply(input: String): BinaryCoding = {
    val bits = input.toList
      .map(value => if(value == '1') One else Zero)

    BinaryCoding(bits)
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