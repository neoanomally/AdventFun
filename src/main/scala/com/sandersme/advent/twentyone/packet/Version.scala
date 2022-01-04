package com.sandersme.advent.twentyone.packet

import com.sandersme.advent.twentyone.binary.{Bit, Bits}

case class Version(bits: Bits) {
  override def toString: String = s"Version: ${Bits.toInt(bits)}"
}

object Version {
  def apply(bit: Bit*): Version = {
    Version(bit.toVector)
  }

}
