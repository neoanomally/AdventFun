package com.sandersme.advent.twentyone.packet

import com.sandersme.advent.twentyone.binary.BinaryCoding
import com.sandersme.advent.twentyone.binary.Bits.parseHex

class LiteralPacketTest extends munit.FunSuite {

  test("Convert a LiteralPacket into an integer with a single nibble") {
    val testPacket = LiteralPacket(Version(Vector(Zero)), PType(Zero), Body(Vector(Nibble(Zero, One, One, Zero))))
    val intResult = testPacket.value

    val expectedResult = BigInt(6)
    assertEquals(intResult, expectedResult)
  }


  test("End to end test on an Literal packet should be parsed from the packet parser") {
    val inputHex = "D2FE28"
    val expectedHexParse: Bits = BinaryCoding.apply("110100101111111000101000").bits.toVector
    val parsedHex = parseHex(inputHex)
    val expectedBinary: Bits = BinaryCoding.apply("011111100101").bits.toVector
    val packet = Packet.fromHex(inputHex)
    val valueInt = packet.head.value

    assertEquals(valueInt, BigInt(2021))
    assertEquals(parsedHex, expectedHexParse)
    assertEquals(packet.head.body.nibbles.flatMap(_.bits), expectedBinary)
  }


  test("Read All Nibbles with at least 15 bits, where the first two are read and no remaining") {
    val testInput: Bits = Vector(One, Zero, Zero, One, One,   One, One, Zero, One, One,      Zero, One, One, Zero, One)
    val expectedNibbles: Nibbles = Vector(Nibble(Zero, Zero, One, One), Nibble(One, Zero, One, One), Nibble(One, One, Zero, One))

    val (nibblesResults: Nibbles, remaining: Bits) = LiteralPacket.readAllNibbles(testInput)

    assertEquals(nibblesResults, expectedNibbles)
    assertEquals(remaining.size, 0)
  }
}
