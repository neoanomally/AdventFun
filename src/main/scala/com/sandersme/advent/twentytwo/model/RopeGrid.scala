package com.sandersme.advent.twentytwo.model

import com.sandersme.advent.twentytwo.model.Direction.negativeDirection

import scala.collection.immutable.::


case class RopeGrid(headCoord: Coord, knotsCoord: List[Coord], tailVisited: Set[Coord],
                    ropeInstructions: RopeInstructions) {

  /**
   * We want to take the next set of instructions. Move the head & potentially the tail, one step at a time
   * until both the head and tail reach the final spot. Each step we update the tailVisited spot so that it
   * includes all new visited places.
   *
   * @return
   */
  def incrementInstruction: RopeGrid = {
    val nextInstruction = ropeInstructions.instructions.head
    val updatedInstructions = ropeInstructions.instructions.tail

    val moveInX = Direction.isMoveX(nextInstruction.direction)
    val stepMovement = nextInstruction.stepMovement

    val startOfFold = (headCoord, knotsCoord, tailVisited)

    val (finalHead, finalTailKnots, updatedVisited) = (0 until  nextInstruction.step)
      .foldLeft(startOfFold){ case ((head, tailKnots, visited), _) =>

        val updatedHead = Coord.updateCoordWithStep(head, moveInX, stepMovement)
        val updatedTail: List[Coord] = Coord.incrementTail(updatedHead, tailKnots)
        val updatedVisited = visited + updatedTail.last

        (updatedHead, updatedTail, updatedVisited)
      }

    RopeGrid(finalHead, finalTailKnots, updatedVisited, RopeInstructions(updatedInstructions))
  }

  def incrementAllInstructions: RopeGrid = {
    ropeInstructions.instructions.indices.foldLeft(this){ (ropeGrid, _) =>
      ropeGrid.incrementInstruction
    }
  }

  def numPlacesTailVisited: Int = tailVisited.size
}

object RopeGrid {
  def parseInput(args: List[String], numKnots: Int = 1): RopeGrid = {
    val instructions = RopeInstructions.parseInstructions(args)
    val head = Coord(0, 0)

    val knots = List.fill(numKnots)(Coord(0, 0))
    val tailVisited = Set(Coord(0,0))

    RopeGrid(head, knots, tailVisited, instructions)
  }
}
