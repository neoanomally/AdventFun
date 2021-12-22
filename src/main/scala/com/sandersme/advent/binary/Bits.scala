package com.sandersme.advent.binary

import com.sandersme.advent.binary.{Bit, One, Zero}


object Bits {
  def parseBinary(input: String): List[Bit] = {
    val bits = input.toList
      .map(value => if(value == '1') One else Zero)

    bits
  }
}