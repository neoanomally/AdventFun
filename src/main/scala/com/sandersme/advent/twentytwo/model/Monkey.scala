package com.sandersme.advent.twentytwo.model

import scala.annotation.tailrec
import scala.collection.mutable.ListBuffer


sealed trait Operation
case class Plus(amount: Option[Long]) extends Operation
case class Multiply(amount: Option[Long]) extends Operation

case class TestDivisible(amount: Long)

type MonkeyId = Int

/**
 * TODO: The totalItemsInspected might start at zero?
 *
 * Each Monkey is going to have a list of items;
 * has an operation that contains a value.
 *
 * Has two monkeys it can throw to, one if false, one if true
 */
case class Monkey(id: MonkeyId, items: List[Long],
                  op: Operation, test: TestDivisible,
                  throwTrueId: MonkeyId, throwFalseId: MonkeyId,
                  totalItemsInspected: Long) {
  def testDivisible(worryLevel: Long): MonkeyId = {
    val isDivisible = (worryLevel % test.amount) == 0

    if isDivisible then
      throwTrueId
    else
      throwFalseId
  }

  def updateWorryLevel(worryLevel: Long): Long = {
    op match {
      case Plus(amnt) => worryLevel + amnt.getOrElse(worryLevel)
      case Multiply(amnt) => worryLevel * amnt.getOrElse(worryLevel)
    }
  }
}

object Monkey {

  def calculateLeastCommonMultiple(monkies: List[Monkey]): Int = {
    monkies.map(_.test.amount).product.intValue
  }

  def iterateMonkeyInTheMiddleNTimes(monkies: List[Monkey], N: Int,
                                     worryDecreaseFactor: Int): List[Monkey] = {
    (1 to N)
      .foldLeft(monkies)((updatedMonkies, _) =>
        oneIterationMonkeyInTheMiddle(updatedMonkies, worryDecreaseFactor))
  }

  def oneIterationMonkeyInTheMiddle(monkies: List[Monkey],
                                    worryDecreaseFactor: Int = 3): List[Monkey]
  = {
    val (_, processed) = _processMonkies(monkies, worryDecreaseFactor = worryDecreaseFactor)

    processed
  }

  /**
   * After each monkey inspects an item but before it tests your worry level, your relief that the
   * monkey's inspection didn't damage the item causes your worry level to be divided by three and
   * rounded down to the nearest integer. There is an assumption here that the monkey never
   * throws to themselves. If that happens it will just fall on the floor (e.g. not be updated
   * anywhere)
   *
   * // TODO: This is a small set of monkeies to process so this shouldn't really have huge
   * // performance considerations. If it was a large list I'd flip to have a map of
   * // monkeies. And increment monkey id to figure out which ones to update.
   * // This is a micro optimization though. It probably would have been better tod o that to begin
   * // with; but I started down this path first. :sweat:
   *
   * @param toProcess
   * @param processed
   * @return
   */
  @tailrec
  def _processMonkies(toProcess: List[Monkey],
                      processed: List[Monkey] = List.empty,
                      worryDecreaseFactor: Int = 3): (List[Monkey], List[Monkey]) = {
    if (toProcess.isEmpty) {
      (toProcess, processed)
    } else {
      val processing = toProcess.head
      val nextToProcessing = toProcess.tail

      // TODO: this could potentially be it's own method.
      val itemWorryLevelToThrow = processing.items
        .map(processing.updateWorryLevel)
        .map(worryLevel => decreaseWorryLevel(worryLevel, worryDecreaseFactor))
        .map(worryLevel => processing.testDivisible(worryLevel) -> worryLevel)
        .groupBy(_._1)
        .map { case (monkeyId, values) => monkeyId -> values.map(_._2) }
        .toMap

      val curriedUpdate = (monkey: Monkey) => updateMonkey(itemWorryLevelToThrow, monkey)


      val currentMonkeyUpdated = processing.copy(items = List.empty, totalItemsInspected =
        processing.totalItemsInspected + processing.items.size)

      val updatedToProcess = toProcess.tail.map(curriedUpdate)
      val updateProcessed = processed.map(curriedUpdate) :+ currentMonkeyUpdated

      _processMonkies(updatedToProcess, updateProcessed, worryDecreaseFactor)
    }
  }

  def decreaseWorryLevel(worryLevel: Long, worryFactor: Int): Long = {
    if (worryFactor == 3) {
      worryLevel / worryFactor
    } else {
      worryLevel % worryFactor
    }
  }

  /**
   * This updates the monkey with the new items.
   *
   * TODO: Update the number of items touched
   * @param itemWorryLevelToThrow
   * @param monkey
   * @return
   */
  def updateMonkey(itemWorryLevelToThrow: Map[MonkeyId, List[Long]], monkey: Monkey): Monkey = {
    if (itemWorryLevelToThrow.contains(monkey.id)) {
      val itemsToAppend: List[Long] = itemWorryLevelToThrow(monkey.id)

      val updatedItems = monkey.items ++ itemsToAppend
      monkey.copy(items = updatedItems)
    } else {
      monkey
    }
  }

  /**
   * This is not a very robust inputFile parser, but it will do the job
   * There is little to no error handling. This is a much more specific type of input
   * @param inputList
   */
  def parseInput(inputList: List[String]): List[Monkey] = {
    val inputIter = inputList.iterator
    val monkeys: ListBuffer[Monkey] = new ListBuffer[Monkey]()

    while (inputIter.hasNext) {
      val monkeyId = parseMonkeyID(inputIter.next())
      val startingItems = parseStartingItems(inputIter.next())
      val operation = parseOperation(inputIter.next())
      val testDivisible = parseTestDivisible(inputIter.next())
      val trueThrow = parseMonkeyToThrow(inputIter.next())
      val falseThrow = parseMonkeyToThrow(inputIter.next())

      val monkey = Monkey(monkeyId, startingItems, operation, testDivisible, trueThrow,
        falseThrow, 0)

      monkeys += monkey
      // This is to chomp the next input line.
      if (inputIter.hasNext)
        inputIter.next()
    }

    monkeys.toList
  }

  def parseMonkeyID(monkeyInput: String): MonkeyId = {
    Integer.parseInt(
      monkeyInput.split(" ")(1)
        .takeWhile(_ != ':')
    )
  }

  // The format for this is: `Starting items: 11, 63, 76
  def parseStartingItems(items: String): List[Long] = {
    val itemIdsCommaString = items.split(":")(1)

    itemIdsCommaString
      .split(",")
      .toList
      .map(_.trim)
      .map(Integer.parseInt)
  }

  def parseOperation(operation: String): Operation = {
    val rightHand = operation.split("= old ")(1).trim
    val split = rightHand.split(" ")

    val value = Option(split(1))
      .filter(_ != "old")
      .map(Integer.parseInt)
      .map(_.toLong)

    split(0) match {
      case "+" => Plus(value)
      case "*" => Multiply(value)
    }
  }

  def parseTestDivisible(testDivisible: String): TestDivisible = {
    val value = Integer.parseInt(
      testDivisible.split("by ")(1))

    TestDivisible(value)
  }

  def parseMonkeyToThrow(input: String): MonkeyId = {
    Integer.parseInt(input.split("monkey ")(1))
  }

  def multiplyBusiestMonkies(monkies: List[Monkey]): Long = {
    monkies
      .map(_.totalItemsInspected)
      .sorted(Ordering[Long].reverse)
      .take(2)
      .product
  }

  def printInspectedItems(monkies: List[Monkey]): Unit = {
    monkies.map(monkey => monkey.id -> monkey.totalItemsInspected)
      .foreach{ case(id, inspected) =>
        println(s"Monkey ${id} inspected ${inspected} items.")
      }
  }
}
