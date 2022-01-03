package com.sandersme.advent.model

case class DeterministicDice(current: Int, timesRolled: Int = 0) {
  def throwDice: (DeterministicDice, Int) = {
    val result = current
    val updatedValue = (current % 100) + 1

    val updatedDice = DeterministicDice(updatedValue, timesRolled + 1)

    (updatedDice, result)
  }
}

object DeterministicDice {
  def defaultDice: DeterministicDice = DeterministicDice(1)

  def throwThreeDice(deterministicDice: DeterministicDice): (DeterministicDice, Int) = {
    val defaultAccum = (deterministicDice, 0)

    (1 to 3).foldLeft(defaultAccum) { case (accum, _) =>
      val thrown = accum._1.throwDice

      (thrown._1, thrown._2 + accum._2)
    }
  }

  extension (deterministicDice: DeterministicDice)
    def rollThreeTimes: (DeterministicDice, Int) = {
      throwThreeDice(deterministicDice)
    }
}
