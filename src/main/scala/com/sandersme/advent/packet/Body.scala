package com.sandersme.advent.packet

import com.sandersme.advent.binary.Nibble

case class Body(nibbles: Nibbles)

object Body {
  def of(nibble: Nibble*): Body = Body(nibble.toVector)
}