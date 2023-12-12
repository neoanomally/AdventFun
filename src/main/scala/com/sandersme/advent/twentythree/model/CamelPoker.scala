package com.sandersme.advent.twentythree.model

import scala.annotation.tailrec

enum CardRank:
  case Joker 
  case One 
  case Two 
  case Three  
  case Four  
  case Five 
  case Six 
  case Seven 
  case Eight 
  case Nine  
  case Ten 
  case Jack 
  case Queen 
  case King 
  case Ace 

enum HandStrength:
  case HighCard 
  case Pair 
  case TwoPair
  case ThreeOfKind 
  case FullHouse 
  case FourOfKind 
  case FiveOfKind 

case class PokerHand(cards: IndexedSeq[CardRank], bet: Int) {
  val handStrength: HandStrength = PokerHand.calculateHandStrength(cards) 
  val handStrengthWithJokers: HandStrength = {
    val updatedCards = updatePokerHandWithJokers
    PokerHand.calculateHandStrength(updatePokerHandWithJokers.cards)
  }
  val containsJoker: Boolean = cards.contains(CardRank.Joker)
    
  def updatePokerHandWithJokers: PokerHand = {
    if (cards.contains(CardRank.Joker)) {
      val updatedCards = PokerHand.replaceAllJokers(cards)
      this.copy(cards = updatedCards)
    } else {
      this
    }
  }
}

object PokerHand {

  def createCardCounts(cards: IndexedSeq[CardRank]): Map[CardRank, Int] = {
    cards.groupMapReduce(identity)(_ => 1)((l, r) => l + r)
  }

  def calculateHandStrength(cards: IndexedSeq[CardRank]): HandStrength = {
    val keyCounts = createCardCounts(cards)
    val maxCount = keyCounts.values.max

    val strength = keyCounts.size match {
      case 1 => HandStrength.FiveOfKind
      case 2 if maxCount == 4 => HandStrength.FourOfKind
      case 2 if maxCount == 3 => HandStrength.FullHouse// only four of a kind or full house
      case 3 if maxCount == 3 => HandStrength.ThreeOfKind
      case 3 if maxCount == 2 => HandStrength.TwoPair
      case 4 => HandStrength.Pair
      case 5 => HandStrength.HighCard
      case _ => throw new Exception("ERROR should only have 1 to five distinct cards")
    }

    strength
  }

  @tailrec
  def replaceAllJokers(cards: IndexedSeq[CardRank], loopInc: Int = 0): IndexedSeq[CardRank] = {
    if (!cards.contains(CardRank.Joker) || loopInc > 5)
      cards
    else {
      val newCard = cardTurnJokerInto(cards)
      val indexOfJoker = cards.indexOf(CardRank.Joker)
      val updatedCards = cards.updated(indexOfJoker, newCard)

      replaceAllJokers(updatedCards, loopInc + 1)
    }
  }

  /**
    * This Method is just a helper method and should be used from replace all jokers
    * only. We should be looking only at the card that we need to turn the joker into by
    * looking at where we are and what we can do to improve. 
    *
    * @param cards
    * @return
    */
  def cardTurnJokerInto(cards: IndexedSeq[CardRank]): CardRank = {
    val cardsWithoutJokers = cards.filter(_ != CardRank.Joker)
    val cardCounts   = PokerHand.createCardCounts(cardsWithoutJokers) // Unoptimized as we are doing this twice lazy val
    val maxCount = cardCounts.values.maxOption

    maxCount match {
      case None       => CardRank.Ace
      case Some(n)    => cardCounts.filter(_._2 == n).keys.maxBy(_.ordinal)
    }
  }
}

object PokerHandOrdering extends Ordering[PokerHand] {

  // There are two steps for comparing two pokerhands.
  // 1. Check the strenght of the hand. 
  // 2a. if one hand is stronger than the other return 1 or -1
  // 2b. if both hands are of equal strength compare each card in order and the one with
  // the highest value is used. 
  override def compare(x: PokerHand, y: PokerHand): Int = {
    val handStrengthCompare = x.handStrengthWithJokers.ordinal compareTo y.handStrengthWithJokers.ordinal

    if (handStrengthCompare == 0) {
      compareCardsUntilOneHandWins(x, y)
    } else {
      handStrengthCompare 
    }
  }

  private def compareCardsUntilOneHandWins(x: PokerHand, y: PokerHand): Int = {
    def loop(currentStep: Int): Int = {
      lazy val comparedCard = x.cards(currentStep).ordinal compareTo y.cards(currentStep).ordinal

      if (currentStep >= x.cards.size)
        0 // Should never get here
      else if (comparedCard != 0) {
        comparedCard
      } else {
        loop(currentStep + 1)
      }
    }
    loop(0)
  }
}

object CamelPoker {
  /**
    * Each line is space delimated. The first column should have 5 characters and digits
    * and the second column should be a bet amount. 
    * AA32K 532   | This is an example input. 
    *
    * @param input
    * @return
    */
  def parse(input: List[String], withJoker: Boolean = false): List[PokerHand] = {
    val pokerHands = input
      .map(_.split(" "))
      .map { camelPokerBet =>
        val pokerHand = parsePokerHand(camelPokerBet(0), withJoker)
        val bet = camelPokerBet(1).toInt
        PokerHand(pokerHand, bet)
      }.toList


    pokerHands
  }


  def parsePokerHand(input: String, withJoker: Boolean = false): IndexedSeq[CardRank] = {
    assert(input.length == 5)
    
    val cardRanks = input.map{
      case '1' => CardRank.One
      case '2' => CardRank.Two
      case '3' => CardRank.Three
      case '4' => CardRank.Four
      case '5' => CardRank.Five
      case '6' => CardRank.Six
      case '7' => CardRank.Seven
      case '8' => CardRank.Eight
      case '9' => CardRank.Nine
      case 'T' => CardRank.Ten
      case 'J' if !withJoker => CardRank.Jack
      case 'J' if withJoker => CardRank.Joker
      case 'Q' => CardRank.Queen
      case 'K' => CardRank.King
      case 'A' => CardRank.Ace
      case e: Char => throw new Exception(s"Error Parsing Input: ${e}")
    }

    cardRanks
  }

  def sortAllHands(pokerHands: List[PokerHand]): List[PokerHand] = {
    pokerHands.sorted(PokerHandOrdering)
  }

  def calculateTotalScore(pokerHands: List[PokerHand]): Int = {
    calculateFinalBets(pokerHands).sum
  }

  def calculateFinalBets(pokerHands: List[PokerHand]): List[Int] = {
   sortAllHands(pokerHands)
    .zipWithIndex
    .map{ case (pokerHand, rank) => 
      pokerHand.bet * (rank + 1)
    }
  }
}
