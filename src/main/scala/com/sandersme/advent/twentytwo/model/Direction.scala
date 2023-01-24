package com.sandersme.advent.twentytwo.model

enum Direction {
  case R, L, U, D
}

object Direction {
  def isMoveX(direction: Direction): Boolean = {
    direction == Direction.L || direction == Direction.R
  }

  def isMoveY(direction: Direction): Boolean = {
    !isMoveX(direction)
  }

  def negativeDirection(direction: Direction): Boolean = {
    direction == Direction.D || direction == Direction.L
  }
}