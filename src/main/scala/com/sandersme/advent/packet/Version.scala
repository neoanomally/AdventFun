package com.sandersme.advent.packet

import com.sandersme.advent.binary.{Bit, Bits}

case class Version(bits: Bits)

object Version {
  def apply(bit: Bit*): Version = {
    Version(bit.toVector)
  }
}
