package com.sandersme.advent.twentyone.packet

import com.sandersme.advent.twentyone.binary.{Bit, Bits}

case class PType(bits: Bits) {
  def value: Int = Bits.toInt(bits)
  override def toString: String = value match {
      case 0 => "sum"
      case 1 => "product"
      case 2 => "min"
      case 3 => "max"
      case 4 => s"Type: ${Bits.toInt(bits)}"
      case 5 => "lt"
      case 6 => "gt"
      case 7 => "eq"
    }

}

object PType {
  def apply(bit: Bit*): PType = {
    PType(bit.toVector)
  }

  val TYPE_FOUR = PType(Bits.HEX_BITS_MAP('4').drop(1))
}