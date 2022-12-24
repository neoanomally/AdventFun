package com.sandersme.advent.twentytwo.model


/**
 * Stack will be a list of items. The head of the list will be the top
 * of the stack.
 * @param stack
 */
case class CrateSupply(stackSeq: Seq[CrateStack], allInstructions: List[CrateInstruction]) {
  def readTopCrates: String = {
    stackSeq
      .flatMap(_.stack.headOption)
      .mkString
  }

  def printTopEachStack: String = {
    stackSeq
      .map(_.stack.headOption.getOrElse(" "))
      .mkString
  }

  def moveForwardAllInstructionsPartOne: CrateSupply = {
    val finalStateAfterAllInstructions = allInstructions
      .indices
      .foldLeft(this) { (crateSupply, _) =>
        crateSupply.moveForwardOneInstructionPartOne
      }

    finalStateAfterAllInstructions
  }

  def moveForwardAllInstructionsPartTwo: CrateSupply = {
    allInstructions
      .indices
      .foldLeft(this) {(crateSupply, _) =>
        crateSupply.moveForwardOneInstructionPartTwo
      }
  }

  def moveForwardOneInstructionPartOne: CrateSupply = {
    CrateSupply.moveForwardOneInstruction(this, CrateStack.splitMoveTopN)
  }

  def moveForwardOneInstructionPartTwo: CrateSupply = {
    CrateSupply.moveForwardOneInstruction(this, CrateStack.moveTopNAsIs)
  }
}

case class CrateInstruction(moveN: Int, fromIdx: Int, toIdx: Int) {
  override def toString: String = s"move ${moveN} from ${fromIdx} to ${toIdx}"
}

object CrateSupply {

  /**
   * TODO Read until we get to the numbers
   * if contains [ then it's a crate line.
   * The first line that isn't crate line is the number line
   * Then there is a blank line
   * then we  parse all the movements.
   * @param input
   * @return
   */
  def fromInputSeq(input: List[String]): CrateSupply = {
    """    [D]
       [N] [C]
       [Z] [M] [P]
        1   2   3 """.stripMargin

    val (stackedCrates, remainingLines) = parseAllStackedCrates(input)
    val allInstructions = parseAllInstructions(remainingLines)

    CrateSupply(stackedCrates, allInstructions)
  }

  def parseAllInstructions(moveLines: List[String]): List[CrateInstruction] = {
    moveLines
      .map { line =>
        val split = line.split(" ")
        val numToMove = split(1).toInt
        val fromIdx = split(3).toInt
        val toIdx = split(5).toInt

        CrateInstruction(numToMove, fromIdx, toIdx)
      }
  }


  // TODO: If more time don't rotate a matrix. Instead read everything from bottom to top; left to right.
  def parseAllStackedCrates(lines: List[String]): (Seq[CrateStack], List[String]) = {
    val crateRows: Seq[Seq[Option[Char]]] = lines
      .takeWhile(_.contains("["))
      .map(line => parseLine(line)) // TODO: Need to transpose all Rows into Columns

    val stackedCrates = rotateCrateRows(crateRows)
      .map{stack =>
        val flattenStack = stack.flatten.toList
        CrateStack(flattenStack)
      }

    val remainingLines = lines.drop(crateRows.length + 2)

    (stackedCrates, remainingLines)
  }

  def rotateCrateRows[T](sparseMatrix: Seq[Seq[Option[T]]]): Seq[Seq[Option[T]]] = {
    val rowLength: Int = sparseMatrix.last.length

    val padded = sparseMatrix
      .map(_.padTo(rowLength, None))

    (0 until rowLength).map{j =>
      padded.indices.map(i =>
        padded(i)(j)
      )
    }
  }

  def parseLine(line: String, accumulator: Seq[Option[Char]] = Seq.empty): Seq[Option[Char]] = {
    if (line.length <= 0) {
      accumulator
    } else {
      val nextChar: Option[Char] = line.charAt(1) match {
        case ' '      => None
        case anyOther => Some(anyOther)
      }

      val updatedAccumulator = accumulator :+ nextChar
      val nextIndex = Math.min(4, line.length)

      parseLine(line.substring(nextIndex), updatedAccumulator)
    }
  }

  type MoveFn = (CrateStack, Int) => (CrateStack, CrateStack)

  def moveForwardOneInstruction(crateSupply: CrateSupply, moveFn: MoveFn): CrateSupply = {
    assert(crateSupply.allInstructions.nonEmpty)
    val crateInstruction = crateSupply.allInstructions.head

    assert(crateInstruction.fromIdx <= crateSupply.stackSeq.length)
    assert(crateInstruction.toIdx <= crateSupply.stackSeq.length)

    val srcStack: CrateStack = crateSupply.stackSeq(crateInstruction.fromIdx - 1)
    val toUpdateStack = crateSupply.stackSeq(crateInstruction.toIdx - 1)

    val (itemsToMove, updatedSrcStack) =  moveFn(srcStack, crateInstruction.moveN)

    val updatedDstStack = CrateStack(itemsToMove.stack ++ toUpdateStack.stack)

    val updatedStackSeqA = crateSupply.stackSeq.updated(crateInstruction.fromIdx - 1, updatedSrcStack)
    val updatedStackSeqB = updatedStackSeqA.updated(crateInstruction.toIdx - 1, updatedDstStack)

    CrateSupply(updatedStackSeqB, crateSupply.allInstructions.tail)
  }
}


