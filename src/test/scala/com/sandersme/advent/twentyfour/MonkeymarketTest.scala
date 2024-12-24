package com.sandersme.advent.twentyfour

import scala.collection.mutable.{Map => MMap}

class MonkeyMarketTest extends munit.FunSuite {
  test("TEST EVOLVING A secret numb 123 for the next ten numbers") {
    val map = MMap.empty[(Long, Int), Long]
    val nextTen = (0 until 10).foldLeft((List.empty[Long], 123L)){ case ((results, currSecret), _) =>
      val nextSecret = MonkeyMarket.evolve(currSecret, map)

      ((results :+ nextSecret), nextSecret)
    }

    val expected = List(15887950, 16495136, 527345,704524, 1553684, 12683156, 11100544, 12249484, 7753432, 5908254)
      .map(_.toLong)

      assertEquals(nextTen._1, expected)
  }

  test("Check the secret number of the 2000th entry") {
    val map = MMap.empty[(Long, Int), Long]
    val part2Map = MMap.empty[(Int, Int, Int, Int), Long]
    val evolveTwoThousand = MonkeyMarket.evolveNTimes(1L, 2000, map, part2Map)

    val expectedResutl = 8685429L

    assertEquals(evolveTwoThousand, expectedResutl)
  }
  
  test("Test the sum of test input till the 2000th secret") {
    val (results, _) = TEST_MONKEY.sumOfTwoThousandthSecret
    val expected = 37327623L

    assertEquals(results,expected)
  }

  test("Test to make sure sliding state class works using the example where the Max SHould be Six with a vector of -1, -1, 0, 2") {
    val state = Vector[Int](3, 0, 6, 5, 4, 4, 6, 4, 4, 2).foldLeft(SlidingState.empty) { case (state, next) => 
      state.append(next)
    }

    val expectedMax = 6L
    val expectedResultDIff = Vector(-1, -1, 0, 2)
    // TODO unsure if these need to be changed.
    assertEquals(state.sequenceValue((-1, -1, 0, 2)), expectedMax)
  }

  test("Part two use the test cases to see if the results match up") {
    val testInput = List("1", "2", "3", "2024")

    val testMonkey = MonkeyMarket.parseInput(testInput)

    val (sum, map) = testMonkey.sumOfTwoThousandthSecret

    println("Max: " + MonkeyMarket.calculateMax(map))

    assertEquals(map((-2,1,-1,3)), 23L)
  }

  val TEST_INPUT = """1
        10
        100
        2024""".stripMargin.split("\n").toList.map(_.trim)

  val TEST_MONKEY = MonkeyMarket.parseInput(TEST_INPUT)
}
