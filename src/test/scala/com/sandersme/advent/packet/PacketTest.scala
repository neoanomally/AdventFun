package com.sandersme.advent.packet

import com.sandersme.advent.binary.{Bit, Bits, Nibble, One, Zero}
import com.sandersme.advent.packet.Packet
import com.sandersme.advent.packet.Version

class PacketTest extends munit.FunSuite {

  val TEST_PACKET: Bits = Bits.parseHex("FA125")

  test("validate parsing the version") {
    val version: Version = Packet.readVersion(TEST_PACKET)
    val expected: Version = Version(Vector(One, One, One))

    assertEquals(version, expected)
  }

  test("validate parsing the type of packet") {
    val packetType: PType = Packet.readPacketType(TEST_PACKET)
    val expected = PType(Vector(One, One, Zero))

    assertEquals(packetType, expected)
  }

  test("Read All Nibbles from the rest of a packet on last Nibble.") {
    val testInput: Bits = Vector(Zero, One, One, Zero, Zero, One, Zero, Zero)
    val expectedResults: Nibble = Nibble(Vector(One, One, Zero, Zero))

    val (nibbles: Nibbles, remainingBits: Bits) = LiteralPacket.readAllNibbles(testInput)

    assertEquals(nibbles.head, expectedResults)
    assertEquals(remainingBits.size, 3)
  }


  test("Remove right Padded Zeroes") {
    val testInput: Bits = Vector(Zero, One, Zero, Zero, Zero)
    val expectedOutput: Bits = Vector(Zero, One)

    val results = Packet.removeLeadingZeros(testInput)
    assertEquals(expectedOutput, results)
  }

  test("Read All the packets based on some well formed input taht consists of two operators") {
    val versionAndType = Vector(One, Zero, Zero, One, Zero, Zero)
    val bodyA = Vector(One, Zero, One, Zero, Zero, One, One, One, One, One, Zero, One, Zero, One, One)
    val bodyB = Vector(One, Zero, One, One, Zero, One, One, Zero, Zero, One, Zero, One, Zero, One, One)

    val input: Vector[Bit] = versionAndType ++ bodyA ++ versionAndType ++ bodyB


    assertEquals(input.size, 42)

    val results: List[Packet] = Packet.readPackets(input)

    // May drop the last right one only for LiteralPacket, but we'll see
    val expectedBodyA = Vector(Zero, One, Zero, Zero, One, One, One, One, One, Zero, One, One)
    val expectedBodyB = Vector(Zero, One, One, Zero, One, Zero, Zero, One,One, Zero, One, One)

    assertEquals(results.size, 2)
    assertEquals(results.head.body.nibbles.flatMap(_.bits), expectedBodyA)
    assertEquals(results.last.body.nibbles.flatMap(_.bits), expectedBodyB)
  }

  test("Larger example with an operator packet that contains three sub-packets that are literal") {
    val packets = Packet.fromHex("EE00D40C823060")

    assertEquals(packets.head.packets.size, 3)

    val literalPacketResults = packets.head.packets.map(_.toInt)
    val expectedLiteralValueResults = List(1, 2, 3)
    assertEquals(literalPacketResults, expectedLiteralValueResults)
  }

  test("Operator packet with operator packets calculate sum should equal 16") {
    val packets = Packet.fromHex("8A004A801A8002F478")
    val results = Packet.calculateVersionSum(packets)

    assertEquals(results, 16)
  }

  // This contains an operator with two subpacket operators. Each of which have two literal subpackets
  test("Even higher nested Operator packet that contains lots more versions equal to 23") {
    val packets = Packet.fromHex("C0015000016115A2E0802F182340")
    val results = Packet.calculateVersionSum(packets)

    assertEquals(results, 12)
  }

  test("Another larger that has 5 literal values a sum of 31") {
    val packets = Packet.fromHex("A0016C880162017C3686B18A3D4780")
    val results = Packet.calculateVersionSum(packets)

    assertEquals(results, 31)
  }

  test("Packet containing two sub packets") {
    val packets = Packet.fromHex("620080001611562C8802118E34")
    val results = Packet.calculateVersionSum(packets)
    assertEquals(results, 23)
  }
}
