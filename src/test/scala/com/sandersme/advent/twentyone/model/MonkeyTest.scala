package com.sandersme.advent.twentyone.model

import com.sandersme.advent.twentytwo.model.{Monkey, MonkeyId, Multiply, Plus, TestDivisible}

class MonkeyTest extends munit.FunSuite {

  val testInput: List[String] =
    """Monkey 0:
      |  Starting items: 79, 98
      |  Operation: new = old * 19
      |  Test: divisible by 23
      |    If true: throw to monkey 2
      |    If false: throw to monkey 3
      |
      |Monkey 1:
      |  Starting items: 54, 65, 75, 74
      |  Operation: new = old + 6
      |  Test: divisible by 19
      |    If true: throw to monkey 2
      |    If false: throw to monkey 0
      |
      |Monkey 2:
      |  Starting items: 79, 60, 97
      |  Operation: new = old * old
      |  Test: divisible by 13
      |    If true: throw to monkey 1
      |    If false: throw to monkey 3
      |
      |Monkey 3:
      |  Starting items: 74
      |  Operation: new = old + 3
      |  Test: divisible by 17
      |    If true: throw to monkey 0
      |    If false: throw to monkey 1
      |""".stripMargin
      .split("\n")
      .toList


  val testMonkeis: List[Monkey] = Monkey.parseInput(testInput)

  test("should parse the full input") {

    val parsed = Monkey.parseInput(testInput)

    val expectedMonkeyLast = Monkey(3, List(74), Plus(Option(3)), TestDivisible(17), 0, 1, 0)
    val monkeyTail = parsed.last

    assertEquals(parsed.size, 4)
    assertEquals(monkeyTail, expectedMonkeyLast)

    val expectedMonkey1ThrowTrue = 2
    val expectedMonkey2ThrowFalse = 0
    val resultsMonkey1ThrowTrue = parsed(1).throwTrueId
    val resultsMonkey1ThrowFalse = parsed(1).throwFalseId

    assertEquals(resultsMonkey1ThrowTrue, expectedMonkey1ThrowTrue)
    assertEquals(resultsMonkey1ThrowFalse, expectedMonkey2ThrowFalse)
  }

  test("Test parsing starting items") {
    val startingItemsInput = "  Starting items: 99, 63, 76, 93, 54, 73"

    val expectedResults: List[Long
    ] = List(99, 63, 76, 93, 54, 73)
    val results = Monkey.parseStartingItems(startingItemsInput)

    assertEquals(results, expectedResults)
  }

  test("Test parsing MonkeyID") {
    val inputTest = "Monkey 0:"

    val expectedMonkeyId: MonkeyId = 0
    val results = Monkey.parseMonkeyID(inputTest)

    assertEquals(results, expectedMonkeyId)
  }

  test("Validate can parse operation both with and without old") {
    val withoutOldInput = "  Operation: new = old + 4"
    val withOldInput = "  Operation: new = old * old"

    val expectedValueWithoutOld = Plus(Option(4))
    val expectedWithOld = Multiply(None)

    val resultsWithoutOld = Monkey.parseOperation(withoutOldInput)
    val resultsWithOld = Monkey.parseOperation(withOldInput)

    assertEquals(resultsWithOld, expectedWithOld)
    assertEquals(resultsWithoutOld, expectedValueWithoutOld)
  }

  test("Validate parsing TestDivisible to 19") {
    val testInput = "  Test: divisible by 19"

    val expectedResults = TestDivisible(19)
    val results = Monkey.parseTestDivisible(testInput)

    assertEquals(results, expectedResults)
  }

  test ("Validate MonkeyID to throw item to") {
    val testInputTrue = "   If true: throw to monkey 1"
    val testInputFalse = "   If false: throw to monkey 3"

    val expectedResultsTrue: MonkeyId = 1
    val expectedResultsFalse: MonkeyId = 3

    val resultsTrue = Monkey.parseMonkeyToThrow(testInputTrue)
    val resultsFalse = Monkey.parseMonkeyToThrow(testInputFalse)

    assertEquals(resultsTrue, expectedResultsTrue)
    assertEquals(resultsFalse, expectedResultsFalse)
  }

  test("Check that iterating through all monkies once has the correct values") {
    val iteration = Monkey.oneIterationMonkeyInTheMiddle(testMonkeis)


    val expectedMonkey0RoundOne = Monkey(0, List(20, 23, 27, 26),
      Multiply(Option(19)), TestDivisible(23), 2, 3, 2)

    assertEquals(iteration.head, expectedMonkey0RoundOne)
  }

  test("Run Monkey In the middle 20 times") {
    val updatedMonkies = Monkey.iterateMonkeyInTheMiddleNTimes(testMonkeis, 20, 3)

    // TODO Write an assertion,
    updatedMonkies.foreach(println)
  }

  test("Multiply the top two busiest monkeies should equal 10605") {
    val expectedResult: Long = 10605

    val updatedMonkies = Monkey.iterateMonkeyInTheMiddleNTimes(testMonkeis, 20, 3)
    val calculatedTopTwoMonkies = Monkey.multiplyBusiestMonkies(updatedMonkies)

    assertEquals(calculatedTopTwoMonkies, expectedResult)
  }

  test("Test Monkey Business after 10000 rounds without a reduced WorryFactor") {
    val expectedResult: Long = 2713310158L

    val worryDecreaseFactor: Long
    = Monkey.calculateLeastCommonMultiple(testMonkeis)

    println(worryDecreaseFactor)

    val updatedMonkies = Monkey
      .iterateMonkeyInTheMiddleNTimes(testMonkeis, 10000, worryDecreaseFactor.intValue)
    val calculatedTopTwoMonkies = Monkey.multiplyBusiestMonkies(updatedMonkies)

    assertEquals(calculatedTopTwoMonkies, expectedResult)
  }

}
