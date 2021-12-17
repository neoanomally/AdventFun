package com.sandersme.advent.model

import scala.collection.mutable

type InsertionMap = Map[String, Char]
case class Polymizer(template: List[Char], insertionMap: InsertionMap) {
  def countOccurrences: List[(Char, Long)] = {
    val map: mutable.Map[Char, Long] = mutable.Map.empty

    template.foldLeft(map){ case(accumuMap, value) =>
      if (accumuMap.contains(value)) {
        accumuMap(value) = accumuMap(value) + 1
      } else {
        accumuMap(value) = 1
      }
      accumuMap
    }.toList
  }

  def diffMaxMin: Long = {
    val (min, max) = countOccurrences
      .foldLeft((Long.MaxValue,
        Long.MinValue)) { case ((min, max), (_, currentCount)) =>
        if (currentCount > max) {
          (min, currentCount)
        } else if (currentCount < min) {
          (currentCount, max)
        } else {
          (min, max)
        }
      }

    max - min
  }
}

object Polymizer {
  def parseInput(input: List[String]): Polymizer = {
    val polymerTemplate = input.head

    val insertionRules: Map[String, Char] = input.takeRight(input.length - 2)
      .map(_.split(" -> "))
      .map{ split =>
        split(0) -> split(1).toCharArray.head
      }.toMap

    Polymizer(polymerTemplate.toCharArray.toList, insertionRules)
  }

  def applyInsertionRuleNSteps(polymizer: Polymizer, steps: Int): Polymizer = {
    (1 to steps).foldLeft(polymizer)((accumlatedPolymizer, _) =>
      applyInsertionRulesOnce(accumlatedPolymizer)
    )
  }

  /**
   * We are going to use the sliding method on the list to get out all pairs.
   * We will print only the inserted pair and the tail for each step to prevent
   * duplicates. But to account for this we do need to add the head back to the
   * list.
   */
  def applyInsertionRulesOnce(polymizer: Polymizer): Polymizer = {
    val updatedTemplate = polymizer.template.head :: polymizer.template
      .sliding(2)
      .flatMap{ pair =>
        val lookupPair = pair.mkString("")
        if (polymizer.insertionMap.contains(lookupPair)) {
          val insertionChar = polymizer.insertionMap(lookupPair)
          List(insertionChar, pair.last)
        } else {
          pair
        }
      }.toList


    polymizer.copy(template = updatedTemplate)
  }
}
