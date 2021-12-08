package com.sandersme.advent.model

import com.sandersme.advent.model.LanternFish.{EMPTY_DEFAULT_FISH, LanternFishBucket, mergeAllFish}

class LanternFishTest extends munit.FunSuite {
  val defaultFishBucketsDontUse: LanternFish = LanternFish(List(LanternFishBucket(3, 2L),
    LanternFishBucket(4, 1L), LanternFishBucket(1, 1L), LanternFishBucket(2, 1L)))

  val TEST_INPUT = mergeAllFish(EMPTY_DEFAULT_FISH, defaultFishBucketsDontUse)

  test("Parse Input 3,4,3,1,2 creates five new LanternFish") {
    val input = "3,4,3,1,2"
    val expectedOutput: List[LanternFishBucket] = mergeAllFish(EMPTY_DEFAULT_FISH, TEST_INPUT)
      .fishBuckets
      .sortBy(_.daysBucket)

    val lanternFish: List[LanternFishBucket] = LanternFish
      .fromInput(input)
      .fishBuckets
      .sortBy(_.daysBucket)

    assertEquals(lanternFish, expectedOutput)
  }

  test("Grow all fish by one day") {
    val expectedFishForDayTwoStarting = 1L
    val expectedFishAtDayTwo = 2L

    val numberOfFishDayTwoStarting = TEST_INPUT.fishBuckets.filter(_.daysBucket == 2).head.numberOfFish
    val numberOfFishAfterOneGrowthDayTwo = TEST_INPUT.growAllFish.fishBuckets
      .filter(_.daysBucket == 2).head.numberOfFish

    assertEquals(numberOfFishDayTwoStarting, expectedFishForDayTwoStarting)
    assertEquals(numberOfFishAfterOneGrowthDayTwo, expectedFishAtDayTwo)
  }

  test("Grow Fish by {X} Days Should have 26 Fish after 18 days") {
    val expectedSize = 26L
    val growFishEighteenDays = TEST_INPUT.growAllFishOverTime(18)
    val numberOfFish = growFishEighteenDays.totalNumberOfFish
    val growFishEightyDays = TEST_INPUT.growAllFishOverTime(80)

    val numberOfFishEightyDays = growFishEightyDays.totalNumberOfFish
    val expectedNumberOfFishEightyDays = 5934L

    assertEquals(numberOfFish, expectedSize)
    assertEquals(numberOfFishEightyDays, expectedNumberOfFishEightyDays)
  }


  /** THis will break */
  test("Grow Fish by 256  Days Should have 26984457539 Fish") {
    val growFishEighteenDays = TEST_INPUT.growAllFishOverTime(256)
    val numberOfFish: Long = growFishEighteenDays.totalNumberOfFish

    val expectedSize = 26984457539L

    assertEquals(numberOfFish, expectedSize)
  }
}
