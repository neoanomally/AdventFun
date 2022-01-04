package com.sandersme.advent.twentyone.model

import com.sandersme.advent.twentyone.model.LanternFish.LanternFishBucket
import com.sandersme.advent.twentyone.model.LanternFish.mergeAllFish

import scala.annotation.tailrec

case class LanternFish(fishBuckets: List[LanternFishBucket]) {
  def totalNumberOfFish: Long = fishBuckets.map(_.numberOfFish).sum

  /**
   * move every fish to a different bucket. Then remove the -1 bucket.
   * Then for all the fish that started at zero, we want to add all those fish
   * to bucket 6 and bucket 8
   */
  def growAllFish: LanternFish = {
    val fishThatMovedDownBucket = fishBuckets
      .map(lanternFish => lanternFish.copy(lanternFish.daysBucket - 1))
      .filter(_.daysBucket != -1)

    val fishThatGrow = fishBuckets
      .filter(_.daysBucket == 0)
      .flatMap(lanternFish => List(lanternFish.copy(daysBucket = 6),
        lanternFish.copy(8)))

    mergeAllFish(LanternFish(fishThatMovedDownBucket), LanternFish(fishThatGrow))
  }

  def growAllFishOverTime(days: Int): LanternFish = growFishByXDays(this, days)

  @tailrec
  private def growFishByXDays(lanternFishes: LanternFish, days: Int): LanternFish = {
    if (days <= 0) {
      lanternFishes
    } else {
      growFishByXDays(lanternFishes.growAllFish, days - 1)
    }
  }
}

object LanternFish {
  case class LanternFishBucket(daysBucket: Int, numberOfFish: Long)

  val EMPTY_DEFAULT_FISH: LanternFish = {
    LanternFish((0 to 8).map(day => LanternFishBucket(day, 0)).toList)
  }

  def mergeFish(fishA: LanternFishBucket, fishB: LanternFishBucket): LanternFishBucket = {
    if (fishA.daysBucket != fishB.daysBucket) {
      throw new Exception("ERROR CAN'T MERGE TWO FISH DIFFERENT DAYS")
    }

    val totalFish: Long = fishA.numberOfFish + fishB.numberOfFish
    LanternFishBucket(fishA.daysBucket, totalFish)
  }

  def mergeAllFish(lanternFishA: LanternFish, lanternFishB: LanternFish): LanternFish = {
    val updatedFishBuckets: List[LanternFishBucket] =
      (lanternFishA.fishBuckets ++ lanternFishB.fishBuckets)
        .groupBy(_.daysBucket)
        .toList
        .map{ case(_, fishes) =>
          fishes.reduce(mergeFish)
        }

    LanternFish(updatedFishBuckets)
  }

  /**
   * Grow the lantern fish by decrement it's life span. Then if the timer reaches
   * -1 the lanternFish timer goes back to 6 and creates a new lanternFish with a timer of 8
   */
  def fromInput(input: String): LanternFish = {
    val inputFish = input
      .split(',')
      .map(_.toInt)
      .groupBy(identity)
      .map{ case (day, fish) =>
        LanternFishBucket(day, fish.length.toLong)
      }.toList

    mergeAllFish(LanternFish(inputFish), EMPTY_DEFAULT_FISH)
  }
}
