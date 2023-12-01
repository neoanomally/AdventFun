package com.sandersme.advent.twentythree.model

import com.sandersme.advent.twentythree.model.CalibrationInstruction.findMinAndMax

import scala.annotation.tailrec

case class CalibrationValues(left: Int, right: Int) {
  def number: Int = left * 10 + right
}
case class CalibrationInstruction(line: String) {

  // one, two, three, four, five, six, seven, eight, and nine should be converted into digits
  // This implementation is very inefficient, but it will do.
  def translateLetters: CalibrationInstruction = {
//    val allIndexes = translationMap
//      .keys.map(word => findAllIndexes(line, word))

    val updated = line


    CalibrationInstruction(updated)
  }

  /**
   * This will throw an error if there is no integer in the string
   * due to this is for AoC not going to do anything about it.
   * @return
   */
  def findFrontAndBack: CalibrationValues = {
    @tailrec
    def loop(i: Int, direction: Int): Int = {
      if (line(i).isDigit)
        line(i).asDigit
      else
        loop(i + direction, direction)
    }

    val left = loop(0, 1)
    val right = loop(line.length - 1, -1)
    CalibrationValues(left, right)
  }

  def findFrontAndBack2: CalibrationValues = {
    findMinAndMax(line)
  }
}

object CalibrationInstruction {

  private val translationMap: Map[String, Int] = Map(
    "one" -> 1,
    "two" -> 2,
    "three" -> 3,
    "four" -> 4,
    "five" -> 5,
    "six" -> 6,
    "seven" -> 7,
    "eight" -> 8,
    "nine" -> 9
  )

  def findMinAndMax(input: String): CalibrationValues = {
    val indicies = findAllIndicies(input)
    val min = indicies.minBy(_.index).num
    val max = indicies.maxBy(_.index).num

    CalibrationValues(min, max)
  }

  case class IndexValue(index: Int, num: Int)
  def findAllIndicies(input: String): List[IndexValue] = {
    val textIndicies = translationMap
      .keys
      .map(key => findTextIndicies(input, key))
      .flatMap{ case(word, incidies) =>
        val num: Int = translationMap(word)

        incidies.map(idx => IndexValue(idx, num))
      }.toList

    val numericIndicies = findAllDigitIndicies(input)

    textIndicies ++ numericIndicies
  }

  def findAllDigitIndicies(input: String): List[IndexValue] = {
    @tailrec
    def loop(pos: Int,  results: List[IndexValue]): List[IndexValue] = {
      if (pos >= input.length) {
        results
      } else if (input(pos).isDigit) {
        val updatedResults = results :+ (IndexValue(pos, input(pos).asDigit))
        loop(pos + 1, updatedResults)
      } else {
        loop(pos + 1, results)
      }
    }

    loop(0, List.empty)
  }


  def findTextIndicies(input: String, word: String): (String, List[Int]) = {
    @tailrec
    def loop(pos: Int, results: List[Int]): List[Int] = {

      lazy val substr = input.substring(pos)
      lazy val index = substr.indexOf(word)

      val isAtEnd = pos >= input.length

      if (isAtEnd || index == -1)
        results
      else
        val fullIndex = index + pos
        val updatedPos = pos + index + word.length

        loop(updatedPos, results :+ fullIndex)
    }


    (word, loop(0, List.empty))
  }
}

object TrebuchetCalibration {
  def face: Boolean = true
  def parseInput(input: List[String]): List[CalibrationValues] = {
    input
      .map(CalibrationInstruction.apply)
      .map(_.findFrontAndBack)
  }

  def parseInputTranslated(input: List[String]): List[CalibrationValues] = {
    input
      .map(CalibrationInstruction.apply)
      .map(_.findFrontAndBack2)
  }

  def sumCalibrationNumbers(input: List[CalibrationValues]): Int = {
    input.map(_.number).sum
  }
}
