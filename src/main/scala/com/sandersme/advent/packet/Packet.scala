package com.sandersme.advent.packet

import com.sandersme.advent.binary.{Bit, Bits, Nibble, One, Zero}
import com.sandersme.advent.packet.{Packet, Version}

import scala.annotation.tailrec

trait Packet(version: Version, packetType: PacketType, packetValue: PacketValue)

object Packet {

  /**
   * @param bits The full packet of the bits. This will read just the first
   *             three bits
   * @return First three bits of the packet
   */
  private[packet] def readVersion(bits: Bits): Version = Version(bits.slice(0, 3))

  private[packet] def readPacketType(bits: Bits): PacketType = PacketType(bits.slice(3, 6))

  @tailrec
  private[packet] def readAllNibbles(bits: Bits, accum: Nibbles = Vector.empty): (Nibbles, Bits) = {

    val updatedAccum = accum :+ Nibble(bits.slice(1, 5))
    val remainingBits = bits.drop(5)
    bits.head match {
      case Zero => {
        val minusPaddedZeros: Bits = Packet.removeLeadingZeros(remainingBits)
        (updatedAccum, minusPaddedZeros)
      }
      case One => readAllNibbles(remainingBits, accum)
    }
  }

  /**
   * TODO: We actually need a way to recursively read all the packets
   * So what we'll need to do is return tuples Packets and Remaining Bits
   * */
  private[packet] def recursiveReadPackets(bits: Bits): Packet = {
    ???
  }

  /**
   * Remove the right most leading zeroes... For example if we have 1000
   * then we only want to return 1
   * @param bits input List of Bit.
   * @return the remaining bits that aren't leading zeroes.
   */
  private[packet] def removeLeadingZeros(bits: Bits): Bits = {
    bits.foldRight(Vector.empty[Bit])((bit, accum) =>
      if (accum.isEmpty && bit == Zero) {
        accum
      } else {
        accum :+ bit
      }
    )
  }

}