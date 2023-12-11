package com.sandersme.advent.twentythree.model

import scala.annotation.tailrec
import com.sandersme.advent.twentythree.model.BoatRace.distanceAfterHoldingN

case class BoatRace(raceStats: List[RaceStat]) {
  def multiplyWaysToWin: Long = {
    raceStats.map(BoatRace.calculateNumberWaysToWin)
    .product
  }
}

case class RaceStat(time: Int, distance: Long) {
  // TODO start at the top of the bottom of the 
  // distances and go middle then go half and half until we find the minimum 
  // FInd the minum failure and return one plus that number
  def binarySearchMax(): Long = {
    binarySearch(false)
  }

  def binarySearchMin(): Long = {
    binarySearch(true) 
  }

 /**
  * The fastest way to calculate the number of different ways we can win is by finding the
  * lowest possible number we can use to win or find the maximum possible number in order
  * to win. Given that we are finding the difference we need to add one to the results 
  *
  * @return
  */
  def calculateTotalWaysToWin: Long = {
    binarySearchMax() - binarySearchMin() + 1
  }

  def calculateDistanceAfterHolding(holdingDuration: Long): Long = {
    BoatRace.distanceAfterHoldingN(holdingDuration, time)
  }
  /**
    * This method is going to do a binary search in order to find the lowest or highest
    * number that can be used to find a winning number. Since the input is in the
    * thousands of billions we need to do a binary search to find the lowest and the
    * highest numbers.  
    *
    * @param findMin
    * @return
    */
  def binarySearch(findMin: Boolean): Long = {
    // I think what I'm looking for are when the values equal each other
    @tailrec
    def loop(minVisited: Long, maxVisited: Long, currentHeldDuration: Long): Long = {
      val distanceAfterHolding = calculateDistanceAfterHolding(currentHeldDuration)
      val isCurrentWinning = distanceAfterHolding > distance
      val shouldGoDown = (isCurrentWinning && findMin) || (!isCurrentWinning && !findMin)

      if (Math.abs(minVisited - maxVisited) <= 1)
        if (calculateDistanceAfterHolding(minVisited) > distance) {
          minVisited
        } else {
          maxVisited
        }
      else if (shouldGoDown)
        val nextHeld = currentHeldDuration - ((currentHeldDuration - minVisited) / 2) 
        loop(minVisited, currentHeldDuration, nextHeld)
      else
        // GO up half the distance 
        val nextHeld = currentHeldDuration + ((maxVisited - currentHeldDuration) / 2)
        loop(currentHeldDuration, maxVisited, nextHeld)
    }
    val middle = time / 2

    loop(1, time, middle)
  }
}

object BoatRace {

  def parseBoatRace(input: List[String]): BoatRace = {
    val time = input.head.split(" +").tail
    val distance = input(1).split(" +").tail


    assert(time.size == distance.size)
    val raceStats = for { 
      i <- 0 until time.size
    } yield RaceStat(time(i).toInt, distance(i).toInt)


    BoatRace(raceStats.toList)
  }

  def parseSingleBoatRace(input: List[String]): BoatRace = {
    val time = input.head.replace("Time:", "").replace(" ", "").toInt
    val distance = input(1).replace("Distance:", "").replace(" ", "").toLong

    BoatRace(List(RaceStat(time, distance)))

  }


  def distanceAfterHoldingN(heldDuration: Long, raceEnd: Long): Long = {
    heldDuration * (raceEnd - heldDuration)
  }

  def calculateNumberWaysToWin(stat: RaceStat): Int = {
    (1 to stat.time)
      .map(heldDuration => distanceAfterHoldingN(heldDuration, stat.time))
      .count(_ > stat.distance)
  }
}
