package com.sandersme.advent.twentyfour

import scala.collection.mutable.{Map => MMap}
import com.sandersme.advent.Input


case class SlidingState(values: Vector[Int], diffs: Vector[Int], sequenceValue: Map[(Int, Int, Int, Int), Long]) {

  def append(value: Int): SlidingState = {
    val nextDiff = values.lastOption.map(value - _).getOrElse(0)
    
    val updatedValues = if (values.size < 4) { values :+ value } else { values.tail :+ value }  
    val updatedDiff = if (diffs.size < 4) { diffs :+ nextDiff } else { diffs.tail :+ nextDiff } 

    val updatedSequnceValue = if (updatedValues.size == 4) {
      val key = getDiffKey(updatedDiff)
      val updatedValue = sequenceValue.getOrElse(key, updatedValues.last.toLong)
      sequenceValue + (key -> updatedValue)
    } else {
      sequenceValue
    }

    SlidingState(updatedValues, updatedDiff, updatedSequnceValue)
  }

  def getDiffKey(diffs: Vector[Int]): (Int, Int, Int, Int) = (diffs(0), diffs(1), diffs(2), diffs(3))
}

object SlidingState { 
  def empty: SlidingState = { SlidingState(Vector.empty, Vector.empty, Map.empty) } 
} 

case class MonkeyMarket(secrets: List[Long]) {
  def sumOfTwoThousandthSecret: (Long, MMap[(Int, Int, Int, Int), Long]) = {
    val cache = MMap.empty[(Long, Int), Long]
    val partTwoResults = MMap.empty[(Int, Int, Int, Int), Long]
    val results = secrets.map{ secret =>
      MonkeyMarket.evolveNTimes(secret, 2000, cache, partTwoResults)
    }.sum

    val totalComputations = secrets.size * 2000
    (results, partTwoResults)
  }


  def findBestNumberToSell(input: Vector[Int]): Int = {
    ???
  }
}

object MonkeyMarket {
  def main(args: Array[String]): Unit = {
    val input = Input.readTwentyFourFromResource("day22_input")

    val monkeyMarket = MonkeyMarket.parseInput(input)
    val startTime = System.currentTimeMillis
    val (sum, map) = monkeyMarket.sumOfTwoThousandthSecret
    val endTime = System.currentTimeMillis
    val maxBananas = map.maxBy(_._2)._2
   
    println(f"The sum of the 2000th secret for each monkey is: ${sum} and it took ${endTime - startTime}ms")
    println(f"The maximum number of bananas that we could get is: ${maxBananas}")

  }

  def calculateMax(map: MMap[(Int, Int, Int, Int), Long]): Long = {
    map.maxBy(_._2)._2
  }


  extension (monkeyMarket: MonkeyMarket) {
    def mix(right: Long): Long = {
      MonkeyMarket.mix(monkeyMarket.secrets.head, right)
    }
  }

  def evolveNTimes(secret: Long, n: Int, cache: MMap[(Long, Int), Long], results: MMap[(Int, Int, Int, Int), Long]): Long = {
    val state = SlidingState.empty
    val (finalState, finalValue) = (0 until n).foldLeft((state, secret)) { case ((slidingState, current), _) =>
      // could even create a cache that calculates the end of the evolution, but this
      // should be fine since it takes 6 seconds.
        val  next = cache.getOrElseUpdate((current, -1), evolve(current, cache))

        val updatedState = slidingState.append(next.toInt % 10)
        (updatedState, next)
      }
      
    finalState.sequenceValue.toList.foreach { (key, value) =>
      results += (key -> (results.getOrElse(key, 0L) + value))
    }

    finalValue
  }

  /// TODO add some kind of caching mechanism that if step two exists it will store for
  //level 
  // Cache is Secret, and step
  def evolve(secret: Long, cache: MMap[(Long, Int), Long]): Long = {
    val stepOne = cache.getOrElseUpdate((secret, 0), prune(mix(secret, secret * 64L)))
    val stepTwo = cache.getOrElseUpdate((stepOne, 1), prune(mix(stepOne, stepOne / 32)))
    val stepThree = cache.getOrElseUpdate((stepTwo, 2), prune(mix(stepTwo, stepTwo * 2048)))
    stepThree
  }

  def mix(left: Long, right: Long): Long = {
    left ^ right
  }

  def prune(secret: Long): Long = {
    secret % 16777216L
  }
/**
  * Calculate the result of multiplying the secret number by 64. Then, mix this result into the secret number. Finally, prune the secret number.
Calculate the result of dividing the secret number by 32. Round the result down to the nearest integer. Then, mix this result into the secret number. Finally, prune the secret number.
Calculate the result of multiplying the secret number by 2048. Then, mix this result into the secret number. Finally, prune the secret number.
  *
  * @param in
  */
  def parseInput(in: List[String]): MonkeyMarket = {
    val secrets = in.map(_.toLong)


    MonkeyMarket(secrets)
  }
} 
