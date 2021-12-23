package com.sandersme.advent.binary

import com.sandersme.advent.binary.{Bit, One, Zero}


type Bits = Vector[Bit]

object Bits {
  case class Accum(total: Int = 0, index: Int = 0)

  def toInt(bits: Bits): Int = {
    bits.foldRight(Accum()){ case (bit, accum) =>
        val updatedIndex = accum.index + 1
        bit match {
          case Zero => accum.copy(index = updatedIndex)
          case One =>
            val updatedTotal: Int = Math.pow(2, accum.index).toInt + accum.total
            accum.copy(total = updatedTotal, index = updatedIndex)
        }
      }.total
  }

  lazy val HEX_BITS_MAP: Map[Char, Vector[Bit]] = Map(
    '0' -> Vector(Zero, Zero, Zero, Zero),
    '1' -> Vector(Zero, Zero, Zero, One),
    '2' -> Vector(Zero, Zero, One, Zero),
    '3' -> Vector(Zero, Zero, One, One),
    '4' -> Vector(Zero, One, Zero, Zero),
    '5' -> Vector(Zero, One, Zero, One),
    '6' -> Vector(Zero, One, One, Zero),
    '7' -> Vector(Zero, One, One, One),
    '8' -> Vector(One, Zero, Zero, Zero),
    '9' -> Vector(One, Zero, Zero, One),
    'A' -> Vector(One, Zero, One, Zero),
    'B' -> Vector(One, Zero, One, One),
    'C' -> Vector(One, One, Zero, Zero),
    'D' -> Vector(One, One, Zero, One),
    'E' -> Vector(One, One, One, Zero),
    'F' -> Vector(One, One, One, One)
  )

  lazy val BITS_HEX_MAP: Map[Vector[Bit], Char] = HEX_BITS_MAP.map(_.swap)

  def parseBinary(input: String): Vector[Bit] = {
    val bits: Vector[Bit] = input
      .map(value => if(value == '1') One else Zero)
      .toVector
    bits
  }

  /**
   *
   * @param input values should be 0 -> F in Hex
   * @return 4 bits representing the hex in binary
   *         TODO For real life this is going to potentially
   *         have some failures, we'd need to be able to handle input
   *         failure cases.
   */
  def parseHex(input: String): Bits = {
    input.flatMap(HEX_BITS_MAP).toVector
  }

}