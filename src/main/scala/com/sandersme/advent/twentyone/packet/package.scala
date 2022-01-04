package com.sandersme.advent.twentyone

package object packet {
  export com.sandersme.advent.twentyone.binary.{Bit, Zero, One, Nibble}

  type Bits = Vector[Bit]
  type Nibbles = Vector[Nibble]
}
