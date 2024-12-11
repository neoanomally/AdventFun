package com.sandersme.advent.twentyfour

import com.sandersme.advent.Input

case class PlutonianPebbles(stones: List[BigInt], stoneCounts: Map[BigInt, BigInt]) {
  def numStones = stones.size
  def numStonesFromMap = stoneCounts.map(_._2).sum
  def distinctStones = stones.toSet.size

  def blinkNTimes(n: Int): PlutonianPebbles = {
    (0 until n).foldLeft(this) { case (accum, _) =>
      accum.blink
    }
  }
  
  def blinkWithMapNTimes(n: Int): PlutonianPebbles = {
    (0 until n).foldLeft(this) { case (accum, _) =>
      accum.blinkWithMap
    }
  }

  def blinkWithMap: PlutonianPebbles = {
    val updates: Iterable[(BigInt, BigInt)] = stoneCounts
      .map{ case (stone, count) => 
        val numDigits = PlutonianPebbles.countDigits(stone)
        val res: List[(BigInt, BigInt)] = if (stone == 0) {
          List(stone + 1 -> count)
        } else if (numDigits % 2 == 0) {
          val remaining = numDigits / 2

          PlutonianPebbles.splitStone(stone, numDigits).map(_ -> count) 
        } else {
          List(2024 * stone -> count)
        }
        res
      }.flatten 

      val updatedStoneCounts = updates.groupMapReduce(_._1)(_._2)(_ + _)
      this.copy(stoneCounts = updatedStoneCounts)
   
  }

  def blink: PlutonianPebbles = {
    val updatedStones = stones.flatMap { stone =>
      val numDigits = PlutonianPebbles.countDigits(stone)
      if (stone == 0) {
        List(stone + 1)
      } else if (numDigits % 2 == 0) {
        val remaining = numDigits / 2

        PlutonianPebbles.splitStone(stone, numDigits)
      } else {
        List(2024 * stone)
      }
    }

    this.copy(stones = updatedStones)
  }
}

object PlutonianPebbles {
  def splitStone(value: BigInt, numDigits: Int): List[BigInt] = {
    val multiplier = createMultiplier(numDigits)
    val left = value / multiplier
    val right = value - (left * multiplier)
    // 435820 -  ((435820 / 1000) * 1000)
    List(left, right)
  }

  def createMultiplier(digits: Int): Int = {
    (0 until (digits / 2)).foldLeft(1) { case (sum, _) => 
      sum * 10
    }
    
  }

  def evenNumDigits(value: BigInt): Boolean = {
    countDigits(value) % 2 == 0
  }

  def countDigits(value: BigInt, digits: Int = 0): Int =  {
    if (value > 0) {
      countDigits(value / 10, digits + 1)
    } else {
      digits
    }
  }

  def main(args: Array[String]): Unit = {

    val input = Input.readTwentyFourFromResource("day11_input")
    val pebbles = PlutonianPebbles.parseInput(input)

    val blink25Times = pebbles.blinkNTimes(25)

    println("The number of stones: " + blink25Times.numStones)
    println("The num distinct stones " + blink25Times.distinctStones)
    
    val blink75Times = pebbles.blinkWithMapNTimes(75)
    println("The number of stones 75 blinks: " + blink75Times.numStonesFromMap)
  }


  def parseInput(in: List[String]): PlutonianPebbles = {
    val stones = in.head
        .split(" ")
        .map(BigInt.apply)
        .toList
    val counts = stones.groupMapReduce(identity)(_ => BigInt(1))(_ + _)

    PlutonianPebbles(stones, counts)
        
  }
  ///If the stone is engraved with the number 0, it is replaced by a stone engraved with the number 1.
  // If the stone is engraved with a number that has an even number of digits, it is replaced by two
  // stones. The left half of the digits are engraved on the new left stone, and the right half of 
  // the digits are engraved on the new right stone. (The new numbers don't keep extra leading zeroes: 
  // 1000 would become stones 10 and 0.)
  // If none of the other rules apply, the stone is replaced by a new stone; the old stone's
  // number multiplied by 2024 is engraved on the new stone.
}
