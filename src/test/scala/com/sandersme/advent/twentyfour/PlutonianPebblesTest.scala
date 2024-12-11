package com.sandersme.advent.twentyfour

import com.sandersme.advent.twentyfour.PlutonianPebbles.countDigits
import com.sandersme.advent.twentyfour.PlutonianPebbles.createMultiplier
import com.sandersme.advent.twentyfour.PlutonianPebbles.splitStone

class PlutonianPebblesTest extends munit.FunSuite {
  test("Test parseInput") {
    val pebbles = PlutonianPebbles.parseInput(TEST_INPUT)
    val expectedPebbles = List(BigInt(125), BigInt(17))

    assertEquals(pebbles.stones, expectedPebbles)
  }

  test("Test that we can blink one time") {
    val pebbles = PlutonianPebbles.parseInput(TEST_INPUT)

    val expected = List(BigInt(253000), BigInt( 1), BigInt( 7))
    val stones = pebbles.blink.stones
    assertEquals(stones, expected)
  }

  test("Blink 25 times should equal 55312 stones") {
    val pebbles = PlutonianPebbles.parseInput(TEST_INPUT)

    val results = pebbles.blinkNTimes(25)
      .numStones

    assertEquals(results, 55312)
  }

  test("Count the number of digits in values") {
    val digitsFive = countDigits(55435)
    val digitsSix = countDigits(554356)

    assertEquals(digitsFive, 5)
    assertEquals(digitsSix, 6)
  }

  test("Test ccreating multiplier so that it can be used to split a digit in half") {
    val multiplier = createMultiplier( 6)
    
    assertEquals(multiplier, 1000)
  }

  test("Test that a number will be split into two separate numbers") {
    val split = splitStone(554356, 6)

    assertEquals(split(0), BigInt(554))
    assertEquals(split(1), BigInt(356))
  }

  val TEST_INPUT = """125 17"""
      .stripMargin
      .split("\n")
      .toList
}
