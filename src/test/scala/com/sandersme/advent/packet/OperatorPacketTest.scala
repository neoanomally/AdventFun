package com.sandersme.advent.packet

import com.sandersme.advent.binary.{BinaryCoding, Zero, One}
import com.sandersme.advent.binary.Bits.parseHex

class OperatorPacketTest extends munit.FunSuite {

  test("38006F45291200 hex will parse to where we have an OperatorPacket with two literal subpackets") {
    val packet = Packet.fromHex("38006F45291200").head

    val expectedVersion = Version(Zero, Zero, One )
    val expectedPType   = PType(One, One, Zero)
    val expectedNumberOfPackets = 2

    assertEquals(packet.ptype, expectedPType)
    assertEquals(packet.version, expectedVersion)
    assertEquals(packet.packets.head.value, BigInt(10))
    assertEquals(packet.packets.last.value, BigInt(20))
    assertEquals(packet.packets.size, expectedNumberOfPackets)
  }

}
