package com.sandersme.advent.packet

import com.sandersme.advent.binary.{Bit, Bits}

case class PType(value: Bits)

object PType {
  def apply(bit: Bit*): PType = {
    PType(bit.toVector)
  }

  val TYPE_FOUR = PType(Bits.HEX_BITS_MAP('4').drop(1))
}