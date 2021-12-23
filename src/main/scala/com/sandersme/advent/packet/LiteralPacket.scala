package com.sandersme.advent.packet

import com.sandersme.advent.binary.{Bit, Bits, Nibble, One, Zero}

import scala.annotation.tailrec


/**
 * Literally packets are where the values within a packet is a literal number
 *
 * @param value
 * @param pType
 * @param version
 */
case class LiteralPacket(override val version: Version,
                         override val ptype: PType,
                         override val body: Body) extends Packet {


  override def value: BigInt = {
    Bits.toBigInt(body.nibbles.flatMap(_.bits))
  }

  override def toString: String = s"Value: ${value}"

}

object LiteralPacket {

  /**
   * Given an arbitrary set of well structured Bits that we know are nibbles (size of 5).
   * Read all the nibbles up till the next packet or empty
   *
   * @param bits any number of bits. We require there be at least 5 bits left, because
   *        the first bit is to check whether or not there are more bits within a given
   *        package. There might be a case where we throw an actual error OR drop bits
   * @param accum: This is a way to accumulate the nibbles we create.
   * @return The set of nibbles that we return and the remaining bits, which will be used for
   *         the next packet.
   */
  @tailrec
  private[packet] def readAllNibbles(bits: Bits, accum: Nibbles = Vector.empty): (Nibbles, Bits) = {
    require(bits.size >= 5, s"Error there aren't enough bits to parse operator: ${bits.size} remaining")
    val remainingBits = bits.drop(5)

    bits.headOption match {
      case None       =>  (accum, bits)
      case Some(Zero) =>
        val slicedBits: Vector[Bit] = bits.slice(1, 5)
        val nibble: Nibble = Nibble(slicedBits)
        val updatedAccum = accum :+ nibble
        (updatedAccum, remainingBits)
      case Some(One) =>
        val updatedAccum = accum :+ Nibble(bits.slice(1, 5))
        readAllNibbles(remainingBits, updatedAccum)
    }
  }
}