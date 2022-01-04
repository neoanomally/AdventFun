package com.sandersme.advent.twentyone.model

class DicePlayerTest extends  munit.FunSuite {
  test("Starting players at their starting position, each rolls dice validate in correct space scores") {
    val player1 = DicePlayer(1, 4)
    val player2 = DicePlayer(2, 8)

    val player1Rolls = player1.moveFoward(6)
    val player2Rolls = player2.moveFoward(15)
    val player2RollsAgain = player2Rolls.moveFoward(33)

    assertEquals(player1Rolls.space, 10)
    assertEquals(player1Rolls.score, 10)
    assertEquals(player2Rolls.space, 3)
    assertEquals(player2RollsAgain.score, 9)
    assertEquals(player2RollsAgain.space, 6)
  }
}
