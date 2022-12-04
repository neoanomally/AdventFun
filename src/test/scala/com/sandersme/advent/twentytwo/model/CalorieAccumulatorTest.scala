package com.sandersme.advent.twentytwo.model

class CalorieAccumulatorTest extends munit.FunSuite {
  val TEST_INPUT = Seq("1000","2000", "3000", "", "4000", "", "5000", "6000", "",
    "7000", "8000", "9000", "", "10000")

  test("Test Calorie Accumulator with the input test data 24000") {
    val max = CalorieAccumulator.maxFromInputSequence(TEST_INPUT)

    assertEquals(max, 24000l)
  }
}
