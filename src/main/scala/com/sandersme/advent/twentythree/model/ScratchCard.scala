package com.sandersme.advent.twentythree.model

import scala.annotation.tailrec
import com.sandersme.advent.twentytwo.model.Rucksack.valueFromChar

/**
  * This is a case class that is used to hold both the ID of the game Card as
  * well as the numbers on the left side of the card and the right side of the
  * card. 
  *
  * @param id: Int Id of the game
  * @param left List[Int] values on the left side of the card
  * @param right: List[Int] values on the right side of the card
  */
case class ScratchCard(id: Int, left: List[Int], right: List[Int]) {
  def findWinningTickets: List[Int] = {
    left.filter(right.contains)
  }

  def numberOverlapping: Int = {
    findWinningTickets.size
  }

  def calculateScore: Int = {
    @tailrec
    def loop(currStep: Int, value: Int): Int = {
      if(currStep <= 0)
        value 
      else
        loop(currStep - 1, value * 2)
    }
    val value = numberOverlapping

    if(value <= 1) {
      value
    } else {
      loop(value - 1, 1)
    }
  }
}


/**
  * This is a companion class to the ScratchCard that has some methods to parse
  * the input files for a Scratch Card. It also includes a method to calculate
  * different calculations across a List of Scratch Cardd
  */
object ScratchCard {
  /**
      * Takes in the input from a file or a list of Cards in the following
      * format:
      *
      * Game  1: 23 481 | 23 58
      *
      * Where after the colon has an arbitrary set of numbers that then splits
      * the right side of the card by a '|' character. 
      *
      * @param input
      * @return
      */
  def parseInput(input: List[String]): List[ScratchCard] = {
    input
      .map (_.split(":"))
      .map{ arrr => 
        val leftSide = arrr(0).split(" ")
        val cardNumber = leftSide(leftSide.size - 1).toInt
        
        val listNumbers = arrr(1).trim.split(" \\| ")
        
        val left: List[Int] = parseOneSide(listNumbers(0))
        val right: List[Int] = parseOneSide(listNumbers(1))
          
        ScratchCard(cardNumber, left, right)
      }
  }


  /**
      * The input should be one side of the card which might be single
      * or double digits. The data is formatted such that if it's a
      * single digit there will be two spaces and not just one.
      *
      * @param input
      * @return
      */
  def parseOneSide(input: String): List[Int] = {
    input.trim 
      .split(" ")
      .map(_.trim)
      .filter(_.nonEmpty)
      .map(_.toInt)
      .toList
  }

  /**
      * This calculates the total number score across all ScratchCards. This
      * does this by calling out to {@link ScratchCard#calculateTotalScore() Calc Score }   
      *
      * @param cards
      * @return
      */
  def calculateTotalScore(cards: List[ScratchCard]): Int = {
    cards
      .map(_.calculateScore)
      .sum
  }

  /**
      * Method takes in a list of scratch cards and returns an adjacency map
      * where we have a map from the CameID -> List[CardID]. The adjacency map
      * should only include the winning card numbers. 
      *
      * @param scratchCards
      * @return
      */
  def createAdjacencyMap(scratchCards: List[ScratchCard]): Map[Int, Int] = {
    scratchCards
      .map(card => card.id -> card.findWinningTickets.size)
      .toMap
  }


  /**
      * To find the total score we need to process the cards one at a time, 
      * as we process the cards, cards in the future will populate even more
      * cards. So we need to accumulate the number of copies we are going to
      * have. We will do this by:
      * 1. Create a Map that we keep accumulate values for. 
      * 2. Process the current card to see how many copies we create for the
      *    next (x) cards. So if we have 2 winning numbers we make 2 copies of
      *    the next two cards. Card one starts as one copy.
      * 3. When we start processing the current card we need to see how many
      *    copies exist currently. Map.getOrElse(id) + 1.  
      * 4. Update the map so that the number of copies for future cards is equal
      *    to the number of copies of the current card. 
      * @param scratchCards
      * @param adjacencyMap
      * @return This is an accumulation of all the counts for each of the cards.
      * The cards will have more and more based on the number of previous
      * copies. May need to have the counts be Long. Will find out after wards. 
      */
  def findTotalCopiesPerCard(scratchCards: List[ScratchCard]): Map[Int, Int] = {
    val maxCardId = scratchCards.map(_.id).max
    val defaultMap = scratchCards.map(card => card.id -> 1).toMap

    scratchCards.foldLeft(defaultMap){ case (accumMap, card) => 
      val numWinningCards = card.numberOverlapping
      val numCopiesGenerated = accumMap.getOrElse(card.id, 1)

      val idsToUpdate = (1 to numWinningCards)
        .map(_ + card.id)
        .filter(_ <= maxCardId) // This might be an unnecessary optimization. 
        .toList

      updateMapCounts(idsToUpdate, numCopiesGenerated, accumMap)
    }
  }


  def updateMapCounts(ids: List[Int], countsAdded: Int, 
    mapCounts: Map[Int, Int]): Map[Int, Int] = {
      ids.foldLeft(mapCounts) { case(updatedCounts, id) =>
        val updatedCount = updatedCounts.getOrElse(id, 0) + countsAdded
        val updatedMap: Map[Int, Int] = updatedCounts + (id -> updatedCount) 
        updatedMap
      }
  }

  def sumTotalCopies(scratchCards: List[ScratchCard]): Int = {
    val totalCopiesPerCard = findTotalCopiesPerCard(scratchCards)
    totalCopiesPerCard
      .values
      .sum
  }

}
