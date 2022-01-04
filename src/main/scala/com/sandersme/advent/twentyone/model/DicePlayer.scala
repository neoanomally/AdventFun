package com.sandersme.advent.twentyone.model

case class DicePlayer(player: Int, space: Int, score: Int = 0) {
  def moveFoward(value: Int): DicePlayer = {
    val newSpace = ((space - 1 + value) % 10) + 1
    val newScore = score + newSpace

    this.copy(space = newSpace, score = newScore)
  }
}