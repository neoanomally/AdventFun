package com.sandersme.advent.twentytwo.model

import munit._

class ElvesCalorieCarrierTest extends FunSuite {
  val TEST_INPUT = Seq("1000","2000", "3000", "", "4000", "", "5000", "6000", "",
    "7000", "8000", "9000", "", "10000")

  test("Test the top three elves calories") {
    val topThreeSum = ElvenCalorieCarrier.sumTopThreeFromInputSequence(TEST_INPUT)

    assertEquals(topThreeSum, 45000L)
  }

}
