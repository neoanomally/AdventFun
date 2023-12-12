package com.sandersme.advent.twentythree.model

import com.sandersme.advent.twentythree.model.CardRank._ 
import com.sandersme.advent.twentythree.model.HandStrength._ 
import com.sandersme.advent.twentythree.model.CamelPoker.sortAllHands

class CamelPokerTest extends munit.FunSuite {
  val TEST_INPUT = """32T3K 765
    |T55J5 684
    |KK677 28
    |KTJJT 220
    |QQQJA 483""".stripMargin
      .split("\n")
      .toList


  val parsedData = CamelPoker.parse(TEST_INPUT)
  val parsedCardsWithJokers = CamelPoker.parse(TEST_INPUT, true)

  test("Validate that we can parse the input into the correct format.") {
    val head = parsedData.head
    val expectedHead = PokerHand(IndexedSeq(Three, Two, Ten, Three, King), 765)
    
    assertEquals(head, expectedHead)
  }

  test("Validate that poker hands have the correct handStrength") {
   val expectedHandStrengths = List(Pair, ThreeOfKind, TwoPair, TwoPair, ThreeOfKind)

   val handStrengths = parsedData.map(_.handStrength)

   assertEquals(handStrengths, expectedHandStrengths)
  }

  test("Validate that we can convert Joker into a queen") {
    val queenHand = parsedCardsWithJokers.last
    val fourOfAKind = PokerHand.replaceAllJokers(queenHand.cards)

    val pokerHandStrength = PokerHand.calculateHandStrength(fourOfAKind)
    val expectedPokerHandStrength = FourOfKind
    assertEquals(pokerHandStrength, expectedPokerHandStrength)
  }

  test("Validate the ordering of pokerhands") {
    val sortedHands: List[PokerHand] = CamelPoker.sortAllHands(parsedData)

    val expectedSortedHands: List[PokerHand] = List(
      parsedData(0), parsedData(3), parsedData(2), parsedData(1), parsedData(4)          
    )

    assertEquals(sortedHands, expectedSortedHands)
  }

  test("Calculate the sum of the bets should equal 6440") {
    val expectedResult = 6440
    val results = CamelPoker.calculateTotalScore(parsedData)

    assertEquals(results, expectedResult)
  }

  test("Calculate the sum of the bets with Joker rules should equal 5905") {
    val expectedResult = 5905

    val result = CamelPoker.calculateTotalScore(parsedCardsWithJokers)

    assertEquals(result, expectedResult)
  }
}

