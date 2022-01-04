package com.sandersme.advent.twentyone

import com.sandersme.advent.twentyone.binary.Nibble
import com.sandersme.advent.twentyone.packet.Packet

package object binary {
  type Nibbles = Vector[Nibble]

  type Packets = Vector[Packet]

}
