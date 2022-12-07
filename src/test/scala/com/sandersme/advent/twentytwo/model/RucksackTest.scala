package com.sandersme.advent.twentytwo.model

class RucksackTest extends munit.FunSuite {
  val TEST_INPUT = List("vJrwpWtwJgWrhcsFMMfFFhFp", "jqHRNqRjqzjGDLGLrsFMfFZSrLrFZsSL", "PmmdzqPrVvPwwTWBwg",
    "wMqvLMZHhHMvwLHjbvcjnnSBnvTQFn", "ttgJtRGJQctTZtZT", "CrZsJsPPZsGzwwsLwLmpwMDw")


  test("test input parses all rucksacks") {
    val rucksack = Rucksack.parseSingleInput("vJrwpWtwJgWrhcsFMMfFFhFp")
    val left = "vJrwpWtwJgWr".toSet
    val right = "hcsFMMfFFhFp".toSet
    val expectedRucksack = Rucksack(left, right)

    assertEquals(rucksack, expectedRucksack)
  }

  test("find common item in two rucksacks should be a p") {
    val rucksack = Rucksack.parseSingleInput("vJrwpWtwJgWrhcsFMMfFFhFp")

    val commonItem: Char = rucksack.findCommonItem
    val expectedItem = 'p'

    assertEquals(commonItem, expectedItem)
  }

  test("Test valueFromChar for p should equal 16 and P should equal 42") {
    val p = Rucksack.valueFromChar('p')
    val P = Rucksack.valueFromChar('P')

    assertEquals(p, 16)
    assertEquals(P, 42)

    val L = Rucksack.valueFromChar('L')
    val v = Rucksack.valueFromChar('v')

    assertEquals(L, 38)
    assertEquals(v, 22)

  }

  test("Test the sum of all the values in the rucksack that should sum to 157") {
    val sum = Rucksack.sumFromInput(TEST_INPUT)
    val expectedSum = 157

    assertEquals(sum, expectedSum)
  }

  test("Test that we pull two triplets from input") {
    val triplets = Rucksack.createTripletsFromInput(TEST_INPUT)

    assertEquals(triplets.length, 2)
  }

  test("Find common item between first triplet") {
    val triplets = Rucksack.createTripletsFromInput(TEST_INPUT)
    val headTriplet = triplets.head

    val commonItem = headTriplet.findCommonItem

    val expectedCommonItem = 'r'

    assertEquals(commonItem, expectedCommonItem)
  }

  test("Sum all Rucksack Triplet common items") {
    import Rucksack._

    val tripletSums = Rucksack.createTripletsFromInput(TEST_INPUT)
      .sumCommonItems

    val expectedTotal = 70

    assertEquals(tripletSums, expectedTotal)
  }
}
