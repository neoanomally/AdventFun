package com.sandersme.advent.model

import com.sandersme.advent.model.BinaryCoding.{One, Zero}

class BitTypeCountersTest extends munit.FunSuite {

  test("Add two bit counters together") {
    val bitA = BitTypeCounter(10, 5)
    val bitB = BitTypeCounter(5, 18)

    val expected = BitTypeCounter(15, 23)
    val result = bitA += bitB

    assertEquals(result, expected)
  }

  test("Add two bit counters together") {
    val bitA = BitTypeCounter(10, 5)
    val bitB = BitTypeCounter(5, 18)

    val expected = BitTypeCounter(15, 23)
    val result = bitA += bitB

    assertEquals(result, expected)
  }

  test("Add two bit counters together") {
    val bitA = BitTypeCounter(10, 5)
    val bitB = BitTypeCounter(5, 18)

    val expected = BitTypeCounter(15, 23)
    val result = bitA += bitB

    assertEquals(result, expected)
  }

  test("[Test the sum of two BitCounterLists") {

    val inputTest: List[BinaryCoding] = List(
      BinaryCoding(List(Zero, One, One, Zero, One)),
      BinaryCoding(List(Zero, Zero, One, Zero, One)),
      BinaryCoding(List(One, Zero, Zero, One, Zero))
    )


    val expectedResult: List[BitTypeCounter] = List(
      BitTypeCounter(2, 1), BitTypeCounter(2, 1), BitTypeCounter(1, 2),
      BitTypeCounter(2, 1), BitTypeCounter(1, 2)
    )

    val diagnostics = Diagnostics(inputTest)

    val summedValues = diagnostics.totalBitCounters
    assertEquals(summedValues, expectedResult)
  }

}
