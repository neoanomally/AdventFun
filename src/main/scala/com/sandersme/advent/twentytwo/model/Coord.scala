package com.sandersme.advent.twentytwo.model

import scala.annotation.tailrec


case class Coord(x: Int, y: Int) {
  def moveXForward(step: Int): Coord = {
    Coord(x + step, y)
  }

  def moveYForward(step: Int): Coord = {
    Coord(x, y + step)
  }
}

object Coord {
  def updateCoordWithStep(coord: Coord, moveInX: Boolean, stepMovement: Int): Coord = {
    if (moveInX) {
      coord.moveXForward(stepMovement)
    } else {
      coord.moveYForward(stepMovement)
    }
  }

  def shouldMoveX(headCoord: Coord, tailCoord: Coord): Boolean = {
    val xDiff = Math.abs(headCoord.x - tailCoord.x)

    xDiff > 1
  }

  def shouldMoveY(headCoord: Coord, tailCoord: Coord): Boolean = {
    val yDiff = Math.abs(headCoord.y - tailCoord.y)

    yDiff > 1
  }

  def shouldNotMoveY(headCoord: Coord, tailCoord: Coord): Boolean = {
    !shouldMoveY(headCoord, tailCoord)
  }

  def shouldNotMoveX(headCoord: Coord, tailCoord: Coord): Boolean = {
    !shouldMoveX(headCoord, tailCoord)
  }

  def isNotInRowOrColumn(headCoord: Coord, tailCoord: Coord): Boolean = {
    val isNotInX = headCoord.x != tailCoord.x
    val isNotInY = headCoord.y != tailCoord.y

    isNotInX && isNotInY
  }

  /**
   * Check if the tail is within one edge away from the
   * @return
   */
  def isTailAdjacent(headCoord: Coord, tailCoord: Coord): Boolean = {
    shouldNotMoveX(headCoord, tailCoord) && shouldNotMoveY(headCoord, tailCoord)
  }

  def stepFromDirection(left: Int, right: Int): Int = {
    if (left - right < 0) {
      -1
    } else {
      1
    }
  }

  def incrementTail(headCoord: Coord, tailKnots: List[Coord]): List[Coord] = {
    val updatedKnots = tailKnots
      .scanLeft(headCoord){case (leader, follower) =>
        val isAdjacent = Coord.isTailAdjacent(leader, follower)

        if (isAdjacent) {
          follower
        } else {
          incrementFollowerKnot(leader, follower)
        }
      }.drop(1)


    // TODO: This is a hack to get things to compile fix
    updatedKnots
  }

  def incrementFollowerKnot(leaderKnot: Coord, followerKnot: Coord): Coord = {
    val shouldMoveDiagonal = Coord.isNotInRowOrColumn(leaderKnot, followerKnot)

    if (shouldMoveDiagonal) {
      moveDiagonalTowardsHead(leaderKnot, followerKnot)
    } else if (shouldMoveX(leaderKnot, followerKnot)) {
      val step = stepFromDirection(leaderKnot.x, followerKnot.x)
      followerKnot.moveXForward(step)
    } else {
      val step = stepFromDirection(leaderKnot.y, followerKnot.y)
      followerKnot.moveYForward(step)
    }
  }

  def moveDiagonalTowardsHead(headCoord: Coord, tailCoord: Coord): Coord = {
    val xDiff = (headCoord.x - tailCoord.x) / Math.abs(headCoord.x - tailCoord.x)
    val yDiff = (headCoord.y - tailCoord.y) / Math.abs(headCoord.y - tailCoord.y)

    Coord(tailCoord.x + xDiff, tailCoord.y + yDiff)
  }

  /**
   * TODO: Update Algorithm Otherwise, if the head and tail aren't touching and aren't in the same row or column,
   * the tail always moves one step diagonally to keep up:

   * @param headCoord
   * @param tailCoord
   * @return one or more tailCoords. We want the head to be the most up to date one.
   */
  @deprecated("This was unnecessary. Needed to move each tail one at a time.")
  def moveTailUntilAdjacent(headCoord: Coord, tailCoord: Coord): Coord = {
    moveTailUntilAdjacent(headCoord, List(tailCoord)).head
  }


  @tailrec
  @deprecated("This was unnecessary. Need to increment one at a time and not across a board.")
  private def moveTailUntilAdjacent(headCoord: Coord, tailCoords: List[Coord]): List[Coord] = {
    val currentTail = tailCoords.head

    if (isTailAdjacent(headCoord, currentTail)) {
      tailCoords
    } else {
      val moveInX = shouldMoveX(headCoord, currentTail)
      val updatedTail = if (moveInX) {
        val step = (headCoord.x - currentTail.x) / Math.abs(headCoord.x - currentTail.x)
        val updatedX = currentTail.x + step

        currentTail.copy(x = updatedX)
      } else {
        val step = (headCoord.y - currentTail.y) / Math.abs(headCoord.y - currentTail.y)
        val updatedY = currentTail.y + step

        currentTail.copy(y = updatedY)
      }

      val updatedTailCoords = updatedTail +: tailCoords

      moveTailUntilAdjacent(headCoord, updatedTailCoords)
    }
  }
}