package com.sandersme.advent.twentytwo.model

/** This is just a typeclass around Long type */
case class ElvenCalorieCarrier(totalCalories: Long)

case class ElvesCalorieCarrierAccumulator(elves: List[ElvenCalorieCarrier], currentAccum: Long) {
  def addElf(elvenCalorieCarrier: ElvenCalorieCarrier): ElvesCalorieCarrierAccumulator = {
    val appendedElf = elves :+ elvenCalorieCarrier
    ElvesCalorieCarrierAccumulator(appendedElf, 0L)
  }

  def addAccumulation(calorie: Long): ElvesCalorieCarrierAccumulator = {
    val updatedAccumulation = currentAccum + calorie
    ElvesCalorieCarrierAccumulator(elves, updatedAccumulation)
  }

  def sumFromTopThree: Long = {
    elves
      .sortBy(-_.totalCalories)
      .take(3)
      .map(_.totalCalories)
      .sum
  }
}

object ElvesCalorieCarrierAccumulator {
  def empty: ElvesCalorieCarrierAccumulator = ElvesCalorieCarrierAccumulator(List.empty, 0)

  def createFromInputSequence(lines: Seq[String]): ElvesCalorieCarrierAccumulator = {
    val calorieAccumulator = lines.foldLeft(ElvesCalorieCarrierAccumulator.empty){ (accum, nextVal) =>
      if (nextVal.trim.isBlank)
        val newElf = ElvenCalorieCarrier(accum.currentAccum)
        accum.addElf(newElf)
      else
        accum.addAccumulation(nextVal.toLong)
    }

    val finalElf = ElvenCalorieCarrier(calorieAccumulator.currentAccum)
    ElvesCalorieCarrierAccumulator(calorieAccumulator.elves :+ finalElf, 0L)
  }
}

object ElvenCalorieCarrier {
  /**
   * I'm being lazy here, because I could be more mem efficient by checking if the next elf carrier
   * makes it into the top three. Instead I create an object for every elf. Essentially I would
   * ideally have a topKMonoid. The other thing is I'm generating these objects as I read through
   * the file which is why the accumulator has two concerns.
   *
   * The other thing I don't like is that I'm doing the accumulation and then
   * @param lines
   * @return
   */
  def sumTopThreeFromInputSequence(lines: Seq[String]): Long = {
    val calorieAccumulator = ElvesCalorieCarrierAccumulator.createFromInputSequence(lines)

    calorieAccumulator
      .sumFromTopThree
  }


}
