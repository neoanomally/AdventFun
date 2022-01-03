package com.sandersme.advent.model

import com.sandersme.advent.model.DeterministicDice.throwThreeDice

class DeterministicDiceTest extends munit.FunSuite {
  test("Throw new Dice three times twice") {
    val dice = DeterministicDice.defaultDice

    val (diceThreeTimes, oneRoundResult) = throwThreeDice(dice)
    assertEquals(diceThreeTimes.timesRolled, 3)
    assertEquals(oneRoundResult, 6)
  }

  test("Throw dice past 100, new dice should have 2") {
    val dice = DeterministicDice(99)
    val (_, resultSixTimes) = DeterministicDice.defaultDice.rollThreeTimes
      ._1
      .rollThreeTimes
    val (threeTimesDice, resultA) = dice.rollThreeTimes
    val (sixTimesDice, resultB) = threeTimesDice.rollThreeTimes

    assertEquals(resultA, 200)
    assertEquals(resultB, 9)
    assertEquals(resultSixTimes, 15)
  }
}
