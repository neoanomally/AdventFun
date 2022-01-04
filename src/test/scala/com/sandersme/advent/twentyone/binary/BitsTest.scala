package com.sandersme.advent.twentyone.binary

class BitsTest extends munit.FunSuite {

  test("Parsing hex to List of bits") {
    val input = "FA1"
    val expected = Vector(One, One, One, One, One, Zero, One, Zero, Zero, Zero, Zero, One)
    val results = Bits.parseHex(input)

    assertEquals(results, expected)
  }

}
