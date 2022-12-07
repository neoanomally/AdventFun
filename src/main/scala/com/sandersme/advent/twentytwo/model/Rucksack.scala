package com.sandersme.advent.twentytwo.model

import scala.annotation.tailrec

case class Rucksack(left: Set[Char], right: Set[Char]) {
  def findCommonItem: Char = {
    val optFindFirst = left.find(right.contains)
    assert(optFindFirst.isDefined)
    optFindFirst.get
  }

  def allUniqueItems: Set[Char] = left ++ right

  def contains(char: Char): Boolean = allUniqueItems.contains(char)
}

case class Triplet(left: Rucksack, middle: Rucksack, right: Rucksack) {
  def findCommonItem: Char = {
    val findFirst = left.allUniqueItems
      .find(item => right.contains(item) && middle.contains(item))

    findFirst.get
  }
}


object Rucksack {

  def parseSingleInput(line: String): Rucksack = {
    val splitIdx = line.length / 2
    val leftString = line.substring(0, splitIdx)
    val rightString = line.substring(splitIdx, line.length)

    Rucksack(leftString.toSet, rightString.toSet)
  }

  def sumFromInput(line: Seq[String]): Int = {
    line
      .map(parseSingleInput)
      .map(_.findCommonItem)
      .map(valueFromChar)
      .sum
  }

  @tailrec
  def createAllTriplets(lines: Seq[String], accumulator: Seq[Triplet]): Seq[Triplet] = {
    if (lines.isEmpty) {
      accumulator
    } else {
      assert(lines.length > 2)
      val (head, tail) = lines.splitAt(3)

      val left = Rucksack.parseSingleInput(head.head)
      val middle = Rucksack.parseSingleInput(lines(1))
      val right = Rucksack.parseSingleInput(head(2))

      val newTriplet = Triplet(left, middle, right)
      val updatedAccumulator = accumulator :+ newTriplet
      createAllTriplets(tail, updatedAccumulator)
    }
  }

  def createTripletsFromInput(lines: Seq[String]): Seq[Triplet] = {
    createAllTriplets(lines, Seq.empty)
  }

  extension (triplets: Seq[Triplet]) {
    def sumCommonItems: Int = {
      triplets
        .map(_.findCommonItem)
        .map(Rucksack.valueFromChar)
        .sum
    }
  }

  def valueFromChar(char: Char): Int = {
    if (char.isLower)
      char.toInt - 96
    else
      char.toInt - 38
  }


}
