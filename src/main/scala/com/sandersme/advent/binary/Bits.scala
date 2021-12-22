package com.sandersme.advent.binary

import com.sandersme.advent.binary.{Bit, One, Zero}


object Bits {
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
    'F' -> Vector(One, Zero, Zero, Zero)
  )

  lazy val BITS_HEX_MAP: Map[Vector[Bit], Char] = HEX_BITS_MAP.map(_.swap)



  def parseBinary(input: String): Vector[Bit] = {
    val bits: Vector[Bit] = input
      .map(value => if(value == '1') One else Zero)
      .toVector
    bits
  }

  def parseHex(input: String): Vector[Bit] = {
    input.flatMap(HEX_BITS_MAP).toVector
  }

}