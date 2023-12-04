package com.sandersme.advent.twentytwo.model

class DistressSignalDecoderTest extends munit.FunSuite {
  val TEST_INPUT = """[1,1,3,1,1]
                     |[1,1,5,1,1]
                     |
                     |[[1],[2,3,4]]
                     |[[1],4]
                     |
                     |[9]
                     |[[8,7,6]]
                     |
                     |[[4,4],4,4]
                     |[[4,4],4,4,4]
                     |
                     |[7,7,7,7]
                     |[7,7,7]
                     |
                     |[]
                     |[3]
                     |
                     |[[[]]]
                     |[[]]
                     |
                     |[1,[2,[3,[4,[5,6,7]]]],8,9]
                     |[1,[2,[3,[4,[5,6,0]]]],8,9]"""
    .stripMargin
    .split("\n")
    .toList

  test("Parsing: [1,1,3,1,1]") {
    def p(int: Int): PacketValue = PacketValue(int)
    val input = "[1,1,3,1,1]"
    val packet = PacketType.parse(input)
    val expectedPacket = PacketList(List(p(1), p(1), p(3), p(1),p(1)))

    assertEquals(packet, expectedPacket)
  }

  test("Parse [[4,4],4,4]") {
    val input = "[[4,4],4,4]"
    val packet4 = PacketValue(4)

    val results = PacketType.parse(input)
    val expected = PacketList(List(PacketList(List(packet4, packet4)), packet4, packet4))

    assertEquals(results, expected)
  }

  test("Parse [[1],[2,3,4]]") {
    val input = "[[1],[2,3,4]]"
    val packetValueList = List(PacketValue(2), PacketValue(3), PacketValue(4))
    val packetValueOne = PacketValue(1)

    val expected = PacketList(List(PacketList(List(packetValueOne)),
      PacketList(packetValueList)))

    val parsed = PacketType.parse(input)

    assertEquals(parsed, expected)

  }

  test("Validate that we can Parse the input") {
    val parsedValues = DistressSignalDecoder.parseInput(TEST_INPUT)

  }

  test("Parse a more complex list type") {
    val input = "[1,[2,[3,[4,[5,6,7]]]],8,9]"

    val parsed = PacketType.parse(input)
    val expected = PacketList(List(
      PacketValue(1), PacketList(List(
        PacketValue(2), PacketList(List(
          PacketValue(3), PacketList(List(
            PacketValue(4), PacketList(List(
              PacketValue(5), PacketValue(6), PacketValue(7)
            ))
          ))
        ))
      )), PacketValue(8), PacketValue(9)
    ))

    assertEquals(parsed, expected)
  }


  test ("Validate Packet Pair 1 is correct") {
    val packetValuesLeft = List(1, 1, 3, 1, 1).map(PacketValue.apply)
    val packetValuesRight = List(1, 1, 5, 1, 1).map(PacketValue.apply)

    val packetLeft = PacketList(packetValuesLeft)
    val packetRight = PacketList(packetValuesRight)
    val packetPair = PacketPair(packetLeft, packetRight)

    val correctOrder = packetPair.isCorrectOrder
    val expectedCorrectOrder = true

    assertEquals(correctOrder, expectedCorrectOrder)
  }


  test("Validate we can find all values in order: 1, 2, 3, 4, 5, 6, 7, 8, 9") {
    val input = "[1,[2,[3,[4,[5,6,7]]]],8,9]"

    val parsed = PacketType.parse(input)
    val listOfValues = parsed.findValuesInOrder
    val expectedListOfValues = List(1, 2, 3, 4, 5, 6, 7, 8, 9)

    assertEquals(listOfValues, expectedListOfValues)
  }

  test("Sum of indicies in correct order should equal 13") {
    val allInput = DistressSignalDecoder.parseInput(TEST_INPUT)

    val sum = allInput.sumOfCorrectOrderIndicies
    val expectedSum = 13

    assertEquals(sum, expectedSum)
  }

  test("Find indicies of pairs in the correct order should be 1, 2, 4, 6") {
    val allInput = DistressSignalDecoder.parseInput(TEST_INPUT)

    val allIndiciesCorrectOrder = allInput.findIndiciesCorrectOrder
    val expectedIndicies = List(1, 2, 4, 6)

    assertEquals(allIndiciesCorrectOrder, expectedIndicies)
  }

  test("The following pairs should be in the correct order [[1],[2,3,4]] vs [[1],4]") {
    val leftInput = "[[1],[2,3,4]]"
    val rightInput = "[[1],4]"

    val parsedLeft = PacketType.parse(leftInput)
    val parsedRight = PacketType.parse(rightInput)
    val packetPair = PacketPair(parsedLeft, parsedRight)

    val correctOrder = packetPair.isCorrectOrder
    val expected = true

    assertEquals(correctOrder, expected)
  }

  test("Count depth of [1,[2,[3,[4,[5,6,7]]]],8,9] should equal 5") {
    val input = "[1,[2,[3,[4,[5,6,7]]]],8,9]"

    val parsed = PacketType.parse(input)

    val depth = parsed.countDepth
    val expectedDepth = 5
    assertEquals(depth, expectedDepth)
  }

  test("Validate that we can parse into the tens of digits") {
    val input = "[5,7,10,2]"

    val packet = PacketType.parse(input)
    val expectedPacket = PacketList(List(PacketValue(5), PacketValue(7), PacketValue(10),
      PacketValue(2)))

    assertEquals(packet, expectedPacket)
  }

  test("Parse first line from the input file") {
    val input = "[[4,[1,[]]],[8,3,[[0,2],[5,2,6],[7,0,10,0],2,[5,7,10,2]],[[5,9],5,10,[9,7,7]]]]"
    val packet = PacketType.parse(input)

    println(packet)
  }
}
