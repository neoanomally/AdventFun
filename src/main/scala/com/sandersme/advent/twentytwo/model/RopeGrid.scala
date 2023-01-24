package com.sandersme.advent.twentytwo.model

import com.sandersme.advent.twentytwo.model.Direction.negativeDirection

case class RopeGrid(headCoord: Coord, tailCoord: Coord, tailVisited: Set[Coord],
                    ropeInstructions: RopeInstructions) {

  /**
   * We want to take the next set of instructions. Move the head & potentially the tail, one step at a time
   * until both the head and tail reach the final spot. Each step we update the tailVisited spot so that it
   * includes all new visited places.
   *
   * @return
   */
  def incrementInstruction: RopeGrid = {
    val nextInstruction::updatedInstructions = ropeInstructions.instructions
    val moveInX = Direction.isMoveX(nextInstruction.direction)
    val stepMovement = nextInstruction.stepMovement

    val default = (headCoord, List(tailCoord))
    val (finalHead, tailRecs) = (0 until  nextInstruction.step)
      .foldLeft(default){ case ((head, tail), _) =>
        val tailHead = tail.head
        val updatedHead = Coord.updateCoordWithStep(head, moveInX, stepMovement)

        val incrementedTail = if (Coord.isTailAdjacent(updatedHead, tailHead)) {
          tailHead
        } else {
          Coord.incrementTail(updatedHead, tailHead)
        }

        val updatedTail: List[Coord] = incrementedTail +: tail

        (updatedHead, updatedTail)
      }

    val currentTail = tailRecs.head
    val updatedVisited: Set[Coord] = tailVisited ++ tailRecs

    RopeGrid(finalHead, currentTail, updatedVisited, RopeInstructions(updatedInstructions))
  }

  def incrementAllInstructions: RopeGrid = {
    ropeInstructions.instructions.indices.foldLeft(this){ (ropeGrid, _) =>
      ropeGrid.incrementInstruction
    }
  }

  def isTailAdjacent: Boolean = {
    Coord.isTailAdjacent(headCoord, tailCoord)
  }

  def numPlacesTailVisited: Int = tailVisited.size
}

object RopeGrid {
  def parseInput(args: List[String]): RopeGrid = {
    val instructions = RopeInstructions.parseInstructions(args)
    val head = Coord(0, 0)
    val tail = Coord(0, 0)
    val tailVisited = Set(tail)

    RopeGrid(head, tail, tailVisited, instructions)
  }
}
