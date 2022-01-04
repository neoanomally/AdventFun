package com.sandersme.advent.twentyone.packet

import com.sandersme.advent.twentyone.binary.Bits

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

    val (results: List[Packet], _) = Packet.readPackets(input)

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

    val literalPacketResults = packets.head.packets.map(_.value)
    val expectedLiteralValueResults = List(BigInt(1), BigInt(2), BigInt(3))
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

    assertEquals(results, 23)
  }

  test("Another larger that has 5 literal values a sum of 31") {
    val packets = Packet.fromHex("A0016C880162017C3686B18A3D4780")
    val results = Packet.calculateVersionSum(packets)

    assertEquals(results, 31)
  }

  //  represents an operator packet (version 3) which contains two sub-packets; each sub-packet is an operator packet
  //  that contains two literal values. This packet has a version sum of 12.
  test("Packet containing two sub packets") {
    val packets = Packet.fromHex("620080001611562C8802118E34")
    val results = Packet.calculateVersionSum(packets)
    assertEquals(results, 12)
  }

  test("Packet containing C200B40A82 should find the sum of 1 & 2 returning 3") {
    val packets = Packet.fromHex("C200B40A82")
    val totalValue = Packet.calculateFinalValue(packets)

    assertEquals(totalValue, BigInt(3))
  }

  test("Packet with hex 04005AC33890 should find the product of 6 and 9 returning 54") {
    val packets = Packet.fromHex("04005AC33890")
    val totalValue = Packet.calculateFinalValue(packets)

    assertEquals(totalValue, BigInt(54))
  }

  test("Packet with 880086C3E88112 should find the minimum of 7, 8, 9 returning 7") {
    val packets = Packet.fromHex("880086C3E88112")
    val totalValue = Packet.calculateFinalValue(packets)

    assertEquals(totalValue, BigInt(7))
  }


  test("Packet with hex CE00C43D881120 should find the maximum of 7, 8, 9 returning 9") {
    val packets = Packet.fromHex("CE00C43D881120")
    val totalValue = Packet.calculateFinalValue(packets)

    assertEquals(totalValue, BigInt(9))
  }


  test("Packet with hex D8005AC2A8F0 should result in 1 because left is greater than right") {
    val packets = Packet.fromHex("D8005AC2A8F0")
    val totalValue = Packet.calculateFinalValue(packets)

    assertEquals(totalValue, BigInt(1))
  }


  test("Packet with hex F600BC2D8F should be 0 Since the result isn't greater than") {
    val packets = Packet.fromHex("F600BC2D8F")
    val totalValue = Packet.calculateFinalValue(packets)

    assertEquals(totalValue, BigInt(0))
  }


  test("Packet with hex 9C005AC2F8F0 should find 0 because sub results aren't equal") {
    val packets = Packet.fromHex("9C005AC2F8F0")
    val totalValue = Packet.calculateFinalValue(packets)

    assertEquals(totalValue, BigInt(0))
  }


  test("Packet with hex {9C0141080250320F1802104A08} complex packet") {
    val packets = Packet.fromHex("9C0141080250320F1802104A08")
    val totalValue = Packet.calculateFinalValue(packets)

    assertEquals(totalValue, BigInt(1))
  }
}
