package com.sandersme.advent.twentytwo.model

sealed trait CycleInstruction
case object NoopInstruction extends CycleInstruction
case class AddXInstruction(X: Int) extends CycleInstruction

type CycleInstructions = List[CycleInstruction]


object  CycleInstructions {
  def parseInput(input: List[String]): CycleInstructions = {
    input.map(parseInstruction)
  }

  def parseInstruction(instruction: String): CycleInstruction = {
    if(instruction.startsWith("noop")) {
      NoopInstruction
    } else {
      val v = instruction.split(" ")(1)
      AddXInstruction(v.toInt)
    }
  }

  /**
   * There are definitely a few ways to do this and I think I'm just going to return the list of
   * signal strengths every 20
   * @param instructions
   * @return
   */
  def processInstructions(instructions: CycleInstructions): List[Int] = {
    case class Accumulator(currV: Int = 1, cycle: Int = 1, resultList: List[Int] = List(1))


    val accumulation = instructions.foldLeft(Accumulator()){ case(accumulator, instruction) =>
      val currV = accumulator.currV

      val newCycles = instruction match {
        case NoopInstruction => List(currV)
        case AddXInstruction(x) => List(currV, currV + x)
      }

      val newV = newCycles.last
      val newCycle = accumulator.cycle + newCycles.size
      val newResultsList = accumulator.resultList ++ newCycles

      Accumulator(newV, newCycle, newResultsList)
    }

    accumulation.resultList
  }

  /**
   * This will take in previously processed cycle instructions. Every 20th value we take that index times the value
   * to get the signal strength at that index.
   * @param cycleValues
   * @return
   */
  def generateSignalStrengths(cycleValues: List[Int], interestingSignals: Set[Int] = Set.empty): List[Long] = {
    cycleValues.zipWithIndex
      .map{ case (value, index) => (value, index + 1)}
      .filter{ case (value, index) =>
        interestingSignals.isEmpty || interestingSignals.contains(index)
      }.map{ case(value, index) =>
        (value * index).toLong
      }
  }

  def signalsStrengthFromInput(input: List[String],
                               interestingCycles: Set[Int]): Map[Int, Int] = {
    case class CycleAccumulator(value: Int, cycle: Int, signals: Map[Int, Int])
    val defaultAccumulator = CycleAccumulator(1, 1, Map.empty)

    val finalAccumulator = input.foldLeft(defaultAccumulator){ case (accumulator, line ) =>
      val instruction = parseInstruction(line)
      val currentCycle = accumulator.cycle + 1

      val nextLoopMap = instruction match {
        case NoopInstruction => Map(currentCycle -> accumulator.value)
        case AddXInstruction(x) => Map(currentCycle -> accumulator.value,
          currentCycle + 1 -> (accumulator.value + x))
      }

      val potentialUpdates = nextLoopMap.filter{ case(key, value) => interestingCycles.contains(key) }
      val (cycle, value) = nextLoopMap.maxBy{ case(key, value) => key }

      val updatedSignals = accumulator.signals ++ potentialUpdates

      CycleAccumulator(value, cycle, updatedSignals)
    }

    finalAccumulator.signals
  }
}