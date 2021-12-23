package com.sandersme.advent.packet

import com.sandersme.advent.binary.Nibble
import com.sandersme.advent.binary.Bits

case class Body(nibbles: Nibbles) {
  override def toString: String = s"Body: ${nibbles.flatMap(_.bits)}"
  def toInt: Int = Bits.toInt(nibbles.flatMap(_.bits))
}

object Body {
  def of(nibble: Nibble*): Body = Body(nibble.toVector)
}