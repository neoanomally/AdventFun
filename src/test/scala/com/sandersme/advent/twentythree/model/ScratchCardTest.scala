package com.sandersme.advent.twentythree.model

class ScratchCardTest extends munit.FunSuite {
  
  val TEST_INPUT: List[String] = 
  """Card 1: 41 48 83 86 17 | 83 86  6 31 17  9 48 53
    |Card 2: 13 32 20 16 61 | 61 30 68 82 17 32 24 19
    |Card 3:  1 21 53 59 44 | 69 82 63 72 16 21 14  1
    |Card 4: 41 92 73 84 69 | 59 84 76 51 58  5 54 83
    |Card 5: 87 83 26 28 32 | 88 30 70 12 93 22 82 36
    |Card 6: 31 18 13 56 72 | 74 77 10 23 35 67 36 11"""
      .stripMargin
      .trim()
      .split("\n")
      .toList

  test("Test that I can run inside vim") {
    val scratchCard = ScratchCard.parseInput(TEST_INPUT)

    val expectedCardIdList = List(1, 2, 3, 4, 5, 6)

    val expectedHeadLeft = List(41, 48, 83, 86, 17)
    val expectedHeadRight = List(83, 86, 6, 31, 17, 9, 48, 53)
    
    val cardList = scratchCard.map(_.id)
    val headLeft = scratchCard.head.left
    val headRight = scratchCard.head.right

    assertEquals(cardList, expectedCardIdList)
    assertEquals(headLeft, expectedHeadLeft)
    assertEquals(headRight, expectedHeadRight)
  }


  test("Test that the number of overlapping in Game one is four and game 5 none") {
    val scratchCard = ScratchCard.parseInput(TEST_INPUT)

    val gameOne = scratchCard.head.numberOverlapping
    val gameFive = scratchCard(4).numberOverlapping

    val expectedGameOne = 4 
    val expectedGameFive = 0

    assertEquals(gameOne, expectedGameOne)
    assertEquals(gameFive, expectedGameFive)
  }

  test("Calculate the score based on current step where num is number of overlap") {
    val scratchCard = ScratchCard.parseInput(TEST_INPUT)

    val gameOne = scratchCard.head.calculateScore
    val gameFive = scratchCard(4).calculateScore

    val expectedGameOne = 8
    val expectedGameFive = 0

    assertEquals(gameOne, expectedGameOne)
    assertEquals(gameFive, expectedGameFive)
  }

  test("Calculate the total number of scores on the test cases should equal 13") { 
    val scratchCards = ScratchCard.parseInput(TEST_INPUT)
    
    val expectedScore = 13
    val totalScore = ScratchCard.calculateTotalScore(scratchCards)

    assertEquals(totalScore, expectedScore)
  }

  test("Validate that we can find the total number of winning scratch tickets from adjacency Map") {
    val scratchCards = ScratchCard.parseInput(TEST_INPUT)
    
    val copiesPerCard = ScratchCard.findTotalCopiesPerCard(scratchCards)
    val expectedValues = Map(1 -> 1, 2 -> 2, 3 -> 4, 4 -> 8, 5 -> 14, 6 -> 1)

    assertEquals(copiesPerCard, expectedValues)
  }

  test("Total sum of all cards should equal 30.") {
    val scratchCards = ScratchCard.parseInput(TEST_INPUT)

    val sumOfCards = ScratchCard.sumTotalCopies(scratchCards)
    val expectedSum = 30

    assertEquals(sumOfCards, expectedSum)
  }
}
