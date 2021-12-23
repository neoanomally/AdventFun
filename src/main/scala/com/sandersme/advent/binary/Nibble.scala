package com.sandersme.advent.binary

/**
 * TODO: Not sure how to handle the final nibble, which are actually just the remaining
 * bits. So it's technically not a Nibble.
 * @param bits
 */
case class Nibble(bits: Bits)

object Nibble {
  def apply(bits: Bit*): Nibble = {
    Nibble(bits.toVector)
  }
}