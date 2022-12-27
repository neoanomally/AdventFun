package com.sandersme.advent.twentytwo.model

class CommDecoderTest extends munit.FunSuite {
  test("Test that we can find the correct start positions from the test inputs") {
    val testOneInput = "bvwbjplbgvbhsrlpgdmjqwftvncz" // pos 5
    val testTwoInput = "nppdvjthqldpwncqszvftbrmjlhg" // pos 6
    val testThreeInput = "nznrnfrfntjfmvfwmzdfjlvtqnbhcprsg" // pos 10
    val testFourInput =  "zcfzfwzzqfrljwzlrfnpqdbhtmscgvjw" // pos 11

    val resultsOne = CommDecoder.findStartOfPacket(testOneInput)
    val resultsTwo = CommDecoder.findStartOfPacket(testTwoInput)
    val resultsThree = CommDecoder.findStartOfPacket(testThreeInput)
    val resultsFour = CommDecoder.findStartOfPacket(testFourInput)

    assertEquals(resultsOne, 5)
    assertEquals(resultsTwo, 6)
    assertEquals(resultsThree, 10)
    assertEquals(resultsFour, 11)
  }

  test("Find start of message based on test inputs provided") {
    val testOneInput = "mjqjpqmgbljsphdztnvjfqwrcgsmlb" // pos 19
    val testTwoInput = "bvwbjplbgvbhsrlpgdmjqwftvncz" // pos 23
    val testThreeInput = "nppdvjthqldpwncqszvftbrmjlhg" // pos 23
    val testFourInput =  "nznrnfrfntjfmvfwmzdfjlvtqnbhcprsg" // pos 29
    val testFiveInput = "zcfzfwzzqfrljwzlrfnpqdbhtmscgvjw" // pos 26

    val resultsOne = CommDecoder.findStartOfMessage(testOneInput)
    val resultsTwo = CommDecoder.findStartOfMessage(testTwoInput)
    val resultsThree = CommDecoder.findStartOfMessage(testThreeInput)
    val resultsFour = CommDecoder.findStartOfMessage(testFourInput)
    val resultsFive = CommDecoder.findStartOfMessage(testFiveInput)


    assertEquals(resultsOne, 19)
    assertEquals(resultsTwo, 23)
    assertEquals(resultsThree, 23)
    assertEquals(resultsFour, 29)
    assertEquals(resultsFive, 26)
  }

}
