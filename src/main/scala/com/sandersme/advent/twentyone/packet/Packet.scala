package com.sandersme.advent.twentyone.packet

import com.sandersme.advent.twentyone.binary.Bits
import com.sandersme.advent.twentyone.model

import scala.annotation.tailrec


/**
 * Two known subclasses of the Packet are [[OperatorPacket]] and [[LiteralPacket]]
 *
 * These are the common interfaces shared by each subclass.
 * Each packet has a [[Version]] and [[PType]] Headers. Some packets may themselves have children.
 * This defaults the children packets to empty lists. Some vectors have a Body, and this defaults it
 * to empty.
 *
 * The value method is probably a bad name, but this is just a numerical representation of the packet and
 * it's subpackets.
 *
 * TODO: Note to self this was a missed opportunity to use Unions and Intersection types :shrug:
 */
trait Packet {
  val version: Version
  val ptype: PType

  def packets: List[Packet] = List.empty
  def body: Body = Body(Vector.empty)
  def versionNum: Int = Bits.toInt(version.bits)

  def value: BigInt
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
   * Because [[Packet]] may have children [[Packets]] this method is used to take in bits as well as
   * an accumulator of packets. When we have enough bits to render a packet (bits.size > 6)
   *
   * Both [[OperatorPacket]] and [[LiteralPacket]] have [[Version]] and [[PType]]
   * values. Those are read first and are always the first six bytes of the packet.
   *
   * Then if the [[PType]] has a value of FOUR, these are Literal packets made up of nibbles.
   * Version four packets may have multiple [[LiteralPacket]]. We won't know how many ahead
   * of time since and rely on sliding across the packets in chunks of 5. Once we get to a
   * Chunk that starts with a 0 it indicates that [[LiteralPacket]] is complete.
   * See [[LiteralPacket.readAllNibbles]] for more information.
   *
   * Any other [[PType]] indicates an [[OperatorPacket]] which has it's own way of determining
   * how many packets to read and return. See [[Packet.parseOperatorPackets]] for more information
   *
   * We will return 0...N remaining bits. The remaining bits will only from from Operator packets
   * Because this is a recursive function we need to return remaining bits as well as the children packets collected.
   * */
  private[packet] def readPackets(bits: Bits,
                                  packets: List[Packet] = List.empty,
                                  packetsToRead: Int = Int.MaxValue): (List[Packet], Bits) = {
   if (bits.isEmpty || bits.size <= 6 || packetsToRead <= 0) {
     val bitsRemoveTrailing = if (bits.size <= 6) Bits.empty else bits
     (packets, bits)
   } else {
     val version = readVersion(bits)
     val pType = readPacketType(bits)
     val bitsAfterHeader = bits.drop(6)

     pType.value match {
       // TODO: Move each of these to their own functions
       case 4 =>
         val (nibbles: Nibbles, remainingBits) = LiteralPacket.readAllNibbles(bitsAfterHeader)
         val literalPacket: LiteralPacket = LiteralPacket(version, pType, Body(nibbles))
         val (literalPackets, otherRemainingBits) = readPackets(remainingBits, packets :+ literalPacket, packetsToRead - 1)
         (literalPackets, otherRemainingBits)
       case _ => // OperatorPacket
         val (remainingBits, operatorPacket) = parseOperatorPackets(bitsAfterHeader, version, pType)
         val (operatorPackets, otherRemainingBits) = readPackets(remainingBits, packets :+ operatorPacket, packetsToRead - 1)
         (operatorPackets, otherRemainingBits)
     }
   }
  }

  /**
   * We have two cases below based on the operatorVersion.
   * If we have a Zero we read 15 bits and this represents the number of bits to read through
   * If we have a One it represents the number of subpackets that this will contain.
   *
   * The other thing worth noting is that the operatorVersion does impact whether or not we use remaining
   * bits to create more subpackets. That's because it assumes we are only reading {x} [[Bit]] from
   * the input bits. For case One the method just reads that many packets.
   *
   * @param bits input List of [[Bit]] aliased to [[Bits]]
   * @param version [[Version]] of the incoming parent [[Packet]] that will be constructed
   * @param pType [[PType]] of the incoming parent [[Packet]] that will be constructed
   * @return
   */
  private[packet] def parseOperatorPackets(bits: Bits, version: Version,
                                           pType: PType): (Bits, OperatorPacket) = {
    val operatorVersion = bits.head

    val  (packets: List[Packet], remainingBits: Bits) = operatorVersion match {
      case Zero =>
        val numBitsToRead: Int = Bits.toInt(bits.slice(1, 16))
        val otherBits: Bits =  bits.drop(16)
        val packetBits: Bits = otherBits.take(numBitsToRead)
        val remainingBits: Bits = otherBits.drop(numBitsToRead)
        val (packetsFromBits, _) = readPackets(packetBits)
        (packetsFromBits, remainingBits)
      case One =>
        val numPacketsToRead: Int = Bits.toInt(bits.slice(1, 12))
        val otherBits = bits.drop(12)
        readPackets(otherBits, packetsToRead = numPacketsToRead)
    }

    val operatorPacket = OperatorPacket(version, pType, Vector.empty, packets)
    (remainingBits, operatorPacket)
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

    val (packets, _) = readPackets(bitsFromHex)
    packets
  }

  /**
   * Go through each packet and find the subchildren of the packets. For each sub child add
   * the version of the packet to the accumulator. By the time this method finds all
   * sub children we have the sum of all the [[Version]] headers.
   * @param packets List of [[Packet]]. The root packet should just be one packet with sub packets
   * @param versionAccum This is a simple [[Int]] that accumulates values. This is only good to the upper bounds of Int
   * @return Sum of all [[Packet]] children [[Version]] Headers
   */
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

  /**
   * To calculate the final value of the packet this will rely on the methods in [[LiteralPacket]]
   * and [[OperatorPacket]] that share the [[Packet.value]]... For the Operator packet it may
   * have subpackets that also need to be called. These are essentailly an acyclic graph/Tree of operations
   * SO there should be no cycles. The leaves of the tree should all be literal values.
   *
   * So at each [[OperatorPacket]] in the graph we should finally get to a place where we have children that
   * are [[LiteralPacket]] that can have one of many operations in the [[OperatorPacket.value]] method.
   *
   * So the call below where we map the input packet to value will recurse through all the children's values
   * This is not tailrec optimized, but should be no problem on small trees.
   * @param packets
   * @return
   */
  def calculateFinalValue(packets: List[Packet]): BigInt = {
    val cumulativeResult = packets.map(_.value)
    require(cumulativeResult.size == 1,
      s"Error the cumulativeResult should return only one value and instead returned ${cumulativeResult.size}")

    cumulativeResult.head
  }

  def printPacketHierarchy(packets: List[Packet], ident: Int = 0): Unit = {
    val idention = List.fill(ident)("-").mkString("")

    packets.foreach{ packet =>
      if (packet.packets.isEmpty) {
        println(idention + s" ${packet.value}")
      } else {
        println(idention + s" ${packet.ptype}")
        printPacketHierarchy(packet.packets, ident + 1)
      }
    }
  }
}