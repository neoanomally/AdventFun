package com.sandersme.advent.packet

/**
 * Operator packet isn't defined in part 1, will need to come back and make sure there
 * aren't any specific defintiions.
 *
 * @param value
 * @param version
 * @param pType
 */
case class OperatorPacket(override val version: Version,
                          override val ptype: PType,
                          bits: Bits,
                          override val packets: List[Packet]) extends Packet {

}