package com.sandersme.advent.twentythree.model

import scala.annotation.tailrec


sealed trait PType
case object Gear extends PType
case object Period extends PType
case object PSymbol extends PType
case class PNum(value: Int) extends PType

case class EngineNumber(value: Int, points: List[PartPoint])

object EngineNumber {
  def fromParts(parts: List[Part]): EngineNumber = {
    val total: Int = parts.collect { case Part(_, v: PNum, _) => v.value }
      .reduce((l, r) => l * 10 + r)

    val partPoints = parts.map(_.partPoint)
    EngineNumber(total, partPoints)
  }
}
case class PartPoint(x: Int, y: Int)
case class Part(partPoint: PartPoint, value: PType, neighbors: List[PartPoint]) {
  def isSymbol: Boolean = value match {
    case PSymbol | Gear => true
    case _       => false
  }
  def isGear: Boolean = value match {
    case Gear => true
    case _    => false
  }
}

case class ProcessingState(currentParts: List[Part], stack: List[EngineNumber]) {
  def nextWithPart(part: Part): ProcessingState = {
    part.value match {
      case PSymbol | Period | Gear =>
        if (currentParts.nonEmpty) {
          val engineNumber = EngineNumber.fromParts(currentParts)
          ProcessingState(List.empty, stack :+ engineNumber)
        } else {
          this
        }

      case PNum(number) => this.copy(currentParts = currentParts :+ part)
    }
  }
  def collectEngineNumbers: List[EngineNumber] = {
    if (currentParts.isEmpty) {
      stack
    } else {
      stack :+ EngineNumber.fromParts(currentParts)
    }
  }
}

object ProcessingState {
  def empty: ProcessingState = ProcessingState(List.empty, List.empty)
}

case class EngineSchematic(grid: Vector[Vector[Part]]) {
  def getPart(x: Int, y: Int): Part = {
    grid(y)(x)
  }

  def getPart(partPoint: PartPoint): Part = {
    getPart(partPoint.x, partPoint.y)
  }

  /**
   * The accumulator might be a good State Machine where we have different "Advance" "methods
   *
   * @return
   */
  def findAllNumbers: List[EngineNumber] = {
    @tailrec
    def loopLine(parts: Vector[Part], state: ProcessingState): List[EngineNumber] = {
      parts.headOption match {
        case None => state.collectEngineNumbers
        case Some(part) =>
          loopLine(parts.tail, state.nextWithPart(part))
      }
    }

    grid.flatMap(parts => loopLine(parts, ProcessingState.empty)).toList
  }

  def sumOfValidParts: Int = {
    findAllValidPartNumbers
      .map(_.value)
      .sum
  }

  /**
   * Check every part neighbor and check if it's a symbol.
   * @return
   */
  def findAllValidPartNumbers: List[EngineNumber] = {
    findAllNumbers
      .filter { engineNumber =>
        engineNumber.points
          .map(point => getPart(point.x, point.y))
          .flatMap(_.neighbors)
          .toSet
          .map(neighborPoint => getPart(neighborPoint.x, neighborPoint.y))
          .exists(_.isSymbol)
      }
  }

  /**
   * A Geor Ratio is only valid if there are exactly two parts adjacent to a gear.
   * This ratio is found by multiplying those two numbers together.
   *
   * 1. Find all distinct gears in all valid part numbers.
   * 2. Take those part numbers and loop through each engine number
   * 3. check if engine number neighbor contains that part number.
   * 4. filter only where exactly two part numbers are adjacent to the gear
   *
   * TODO: I could clean this up a bit better. Some refactoring to make this simpler
   * POtentially breaking these down into sub methods.
   */
  def findAllValidGearRatio: List[Int] = {
    val engineNumbers = findAllNumbers

    val allNumberGears: Set[PartPoint] = engineNumbers
      .flatMap(_.points)
      .flatMap(point => getPart(point).neighbors) // get neighbors
      .map(getPart) // get neighbors part
      .filter(_.isGear)
      .map(part => part.partPoint)
      .toSet

    val gearToNumberMap = engineNumbers.flatMap { number =>
      val allPartNeighbors = number.points.map(getPart).flatMap(_.neighbors).toSet
      val hasGearNeighbor = allPartNeighbors.intersect(allNumberGears)

      hasGearNeighbor.map(gear => gear -> number)
    }

    gearToNumberMap
      .groupBy(_._1)
      .map{ case(_, engines) => engines.map(_._2)}
      .filter(_.size == 2)
      .map(_.map(_.value))
      .map(_.product)
      .toList
  }

  def sumOfGearRatios: Int = {
    findAllValidGearRatio.sum
  }
}

object EngineSchematic {
  def parseInput(input: List[String]): EngineSchematic = {
    val maxY = input.length
    val maxX = input.head.length

    val grid: Vector[Vector[Part]] = input.toVector
      .zipWithIndex
      .map((line, y) => line
        .toVector
        .zipWithIndex
        .map { case (cha, x) =>
          val neighbors: List[PartPoint] = findAllNeighbors(x, y, maxX, maxY)
          val pType: PType = cha match {
            case '.' => Period
            case '*' => Gear
            case n if n.isDigit => PNum(n.asDigit)
            case _ => PSymbol
          }

          Part(PartPoint(x, y), pType, neighbors)
        }
      )

    EngineSchematic(grid)
  }

  def findAllNeighbors(currX: Int, currY: Int, maxX: Int, maxY: Int): List[PartPoint] = {
    val generated = for {
      x <- -1 to 1
      y <- -1 to 1
      if ((x != 0) || (y != 0))
    } yield PartPoint(currX + x, currY + y)

    generated
      .filter(point => point.x < maxX && point.y < maxY)
      .filter(point => point.x >= 0 && point.y >= 0)
      .toList
  }
}
