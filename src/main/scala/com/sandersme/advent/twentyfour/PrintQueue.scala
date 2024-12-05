package com.sandersme.advent.twentyfour

import scala.annotation.tailrec
import com.sandersme.advent.Input

case class PrintQueue(dependencies: Map[Int, Set[Int]], updates: List[List[Int]]) {

  def sumNotChanged: Int = {
    updates
      .filter(input => isIdentical(input, sortUpdate(input)))
      .map(update => update(update.length / 2))
      .sum
  }

  def sumChanged: Int = {
    updates
      .map(original => (original, sortUpdate(original)))
      .filter{ case (original, updated) => !isIdentical(original, updated) }
      .map { case (original, updated) => updated }
      .map(update => update(update.length / 2))
      .sum
  }

  @tailrec
  private def isIdentical(left: List[Int], right: List[Int], idx: Int = 0): Boolean = {
    if (idx == left.length) {
      true
    } else if (left(idx) == right(idx)) {
      isIdentical(left, right, idx + 1)
    } else {
      false
    }
  }

  private[this] def sortUpdate(in: List[Int]): List[Int] = {
    in.sortWith { case (left, right) =>
      if (dependencies.contains(left) && dependencies(left).contains(right)) {
        true
      } else {
        false
      }
    }
  }
}

object PrintQueue {
  def parseInput(in: List[String]): PrintQueue = {
    val pairs = in.takeWhile(!_.isEmpty)

    val dependenciesMap = pairs
      .map(_.split("\\|"))
      .map(arr => arr(0).toInt -> arr(1).toInt)
      .groupMap(_._1)(_._2)
      .toMap
      .map{ case (key, values) => key -> values.toSet }

    val instructions = in.drop(pairs.length + 1)
      .map(_.split(",").toList)
      .map(_.map(_.toInt))

    PrintQueue(dependenciesMap, instructions)
  }


  def main(args: Array[String]): Unit = {
    val input = Input.readTwentyFourFromResource("day5_input")

    val pq = PrintQueue.parseInput(input)

    val sumNotChanged = pq.sumNotChanged
    val sumChanged = pq.sumChanged


    println(f"The sum of the not changed values are: $sumNotChanged")
    
    println(f"The sum of the not changed values are: $sumChanged")
  }
}
