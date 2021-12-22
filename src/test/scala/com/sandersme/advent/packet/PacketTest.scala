package com.sandersme.advent.packet

import com.sandersme.advent.binary.{Bit, Bits, One, Zero}
import com.sandersme.advent.packet.Packet

class PacketTest extends munit.FunSuite {

  val TEST_PACKET: Bits = Bits.parseHex("FA125")

  test("validate parsing the version") {
    val version: Version = Packet.readVersion(TEST_PACKET)
    val expected: Version = Version(Vector(One, One, One))

    assertEquals(version, expected)
  }

  test("validate parsing the type of packet") {
    val packetType: PacketType = Packet.readPacketType(TEST_PACKET)
    val expected = PacketType(Vector(One, One, Zero))

    assertEquals(packetType, expected)
  }

  test("Read All Nibbles from the rest of a packet.") {
    ???
  }

  test("Remove right Padded Zeroes") {
    ???
  }
}
