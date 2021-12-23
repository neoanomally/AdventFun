package com.sandersme.advent.packet

import com.sandersme.advent.binary.{Bit, Bits, Nibble, One, Zero}
import com.sandersme.advent.packet.{Packet, Version}

import scala.annotation.tailrec

trait Packet {
  val version: Version
  val ptype: PType

  def packets: List[Packet] = List.empty
  def toInt: Int = Bits.toInt(ptype.value)
  def body: Body = Body(Vector.empty)
  def versionNum: Int = Bits.toInt(version.bits)
}

object Packet {

  /**
   * @param bits The full packet of the bits. This will read just the first
   *             three bits
   * @return First three bits of the packet
   */
  private[packet] def readVersion(bits: Bits): Version = Version(bits.slice(0, 3))

  /**
   * The second set of 3 bits are used to read the packet type.
   * @param bits bits for the overall packet.
   * @return The packet Type.
   */
  private[packet] def readPacketType(bits: Bits): PType = PType(bits.slice(3, 6))

  /**
   * TODO: We actually need a way to recursively read all the packets
   * So what we'll need to do is return tuples Packets and Remaining Bits
   * */
//  @tailrec
  private[packet] def readPackets(bits: Bits, packets: List[Packet] = List.empty): List[Packet] = {
   if (bits.isEmpty || bits.size <= 6)
     packets
   else {
     val version = readVersion(bits)
     val pType = readPacketType(bits)
     val bitsAfterHeader = bits.drop(6)

     pType match {
       // TODO: Move each of these to their own functions
       case PType.TYPE_FOUR =>
         val (nibbles: Nibbles, remainingBits) = LiteralPacket.readAllNibbles(bitsAfterHeader)
         val literalPacket = LiteralPacket(version, pType, Body(nibbles))
         readPackets(remainingBits, packets :+ literalPacket)
       case _ => // OperatorPacket
         val (remainingBits, operatorPacket) = parseOperatorPackets(bitsAfterHeader, version, pType)
         readPackets(remainingBits, packets :+ operatorPacket)
     }
   }
  }

  /**
   * We have two cases below based on the operatorVersion.
   * If we have a Zero we read 15 bits and this represents the number of bits to read through
   * If we have a One it represents the number of subpackets that this will contain.
   *
   * @param bits
   * @param version
   * @param pType
   * @return
   */
  private[packet] def parseOperatorPackets(bits: Bits, version: Version,
                                           pType: PType): (Bits, OperatorPacket) = {
    val operatorVersion = bits.head

    val  packets = operatorVersion match {
      case Zero =>
        val numBitsToRead: Int = Bits.toInt(bits.slice(1, 16))
        val otherBits: Bits =  bits.drop(16)
        val packetBits: Bits = otherBits.take(numBitsToRead)
        val remainingBits: Bits = otherBits.drop(numBitsToRead) // TODO: Do we need these remaining bits?
        readPackets(packetBits)
      case One =>
        val numberOfBits = bits.slice(1, 12)
        val otherBits = bits.drop(12)
        readPackets(otherBits)
    }


    val operatorPacket = OperatorPacket(version, pType, Vector.empty, packets)
    // TODO: FOR NOW RETURNING EMPTY VECTOR DO WE NEED THE BITS FROM ABOVE?
    (Vector.empty, operatorPacket)
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
        bit +: accum
      }
    )
  }

  /**
   * Todo we need to look at the version and return either a literal packet or
   * operator packet, but for now we can throw everything into a operator packet
   * I actually think the bit `chomping` will happen here instead of durint the
   * recursive nibble calls
   *
   * @param version
   * @param pType
   * @param body
   * @return
   */
  def apply(version: Version, pType: PType, nibbles: Nibbles): Packet = {
    val packetBody = Body(nibbles)
    LiteralPacket(version, pType, packetBody)
  }

  def fromHex(input: String): List[Packet] = {
    val bitsFromHex: Bits = Bits.parseHex(input)

    readPackets(bitsFromHex)
  }

  @tailrec
  def calculateVersionSum(packets: List[Packet], versionAccum: Int = 0): Int = {
    val updatedVersionAccum = packets.map(_.versionNum).sum + versionAccum
    val flattenedPackets = packets.flatMap(_.packets)

    if (flattenedPackets.isEmpty) {
      updatedVersionAccum
    } else {
      calculateVersionSum(flattenedPackets, updatedVersionAccum)
    }
  }

}