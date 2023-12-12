package com.sandersme.advent.twentythree.model

class BoatRaceTest extends munit.FunSuite {
  val TEST_INPUT = """Time:      7  15   30
    |Distance:  9  40  200""".stripMargin
    .split("\n")
    .toList

  test("Validate parse out single boat race parses all the spaces into one race stat") {
    val parsedData = BoatRace.parseSingleBoatRace(TEST_INPUT)
  
    val expectedTime = 71530
    val expectedDistance = 940200L
    val raceStat = parsedData.raceStats.head

    assertEquals(raceStat.time, expectedTime)
    assertEquals(raceStat.distance, expectedDistance)
  }

  test("Validate that parse Test input has the correct model of time and distances") {
    val parsedData = BoatRace.parseBoatRace(TEST_INPUT) 

    val expectedTime: List[Int] = List(7, 15, 30) 
    val time = parsedData.raceStats.map(_.time)
    val distance = parsedData.raceStats.map(_.distance)
    val expectedDistance: List[Long] = List(9L, 40L, 200L)

    val expectedStatTwo = RaceStat(15, 40L)
    assertEquals(time, expectedTime)
    assertEquals(distance, expectedDistance)
  }

  test("See if we can get some result from binarySearchMin") {
    val raceStat = RaceStat(7, 9L)
    val min = raceStat.binarySearchMin()
    val max = raceStat.binarySearchMax()
    val expectedMin = 2L
    val expectedMax = 5L
    assertEquals(min, expectedMin)
    assertEquals(max, expectedMax)
  }

  test("calculate the number of ways that can be used to win should equal: 71503") {
    val parsedData = BoatRace.parseSingleBoatRace(TEST_INPUT)
    val numberOfWaysToWin = parsedData.raceStats.head.calculateTotalWaysToWin // There should only be one
    val expectedResult = 71503L

    assertEquals(numberOfWaysToWin, expectedResult)
  }


  test("Calculate max distance travelled if holding for {x} milliseconds") { 
    val expectedDistanceOneMillis = 6L
    val expectedDistanceTwoMillis = 10L
    val expectedDistanceThreeMillis = 12L
    val expectedDistanceFourMillis = 12L
    val expectedDistanceFiveMillis = 10L
    val expectedDistanceSixMillis = 6L
    val expectedDistanceSevenMillis = 0L

    val distanceOneMillis = BoatRace.distanceAfterHoldingN(1, 7)
    val distanceTwoMillis = BoatRace.distanceAfterHoldingN(2, 7)
    val distanceThreeMillis = BoatRace.distanceAfterHoldingN(3, 7)
    val distanceFourMillis = BoatRace.distanceAfterHoldingN(4, 7)
    val distanceFiveMillis = BoatRace.distanceAfterHoldingN(5, 7)
    val distanceSixMillis = BoatRace.distanceAfterHoldingN(6, 7)
    val distanceSevenMillis = BoatRace.distanceAfterHoldingN(7, 7)

    assertEquals(distanceOneMillis , expectedDistanceOneMillis)
    assertEquals(distanceTwoMillis , expectedDistanceTwoMillis)
    assertEquals(distanceThreeMillis, expectedDistanceThreeMillis)
    assertEquals(distanceFourMillis , expectedDistanceFourMillis)
    assertEquals(distanceFiveMillis, expectedDistanceFiveMillis)
    assertEquals(distanceSixMillis, expectedDistanceSixMillis)
    assertEquals(distanceSevenMillis, expectedDistanceSevenMillis)
  }

  test("Calculating the number of ways to win the race on test data should equal 288") {
    val boatRace = BoatRace.parseBoatRace(TEST_INPUT)

    val numberOfWaysToWin = boatRace.multiplyWaysToWin
    val expectedValue = 288L

    assertEquals(numberOfWaysToWin, expectedValue)
  }
  
}
