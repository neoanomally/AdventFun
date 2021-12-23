package com.sandersme.advent

import com.sandersme.advent.binary._

package object packet {
  export com.sandersme.advent.binary.{Bit, Zero, One, Nibble}

  type Bits = Vector[Bit]
  type Nibbles = Vector[Nibble]
}
