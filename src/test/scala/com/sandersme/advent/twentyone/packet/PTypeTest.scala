package com.sandersme.advent.twentyone.packet

class PTypeTest extends munit.FunSuite {
  test("Validate that version 4 bits are equal to 100") {
    assertEquals(PType.TYPE_FOUR, PType(Vector(One, Zero, Zero)))
  }
}
