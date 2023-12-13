package com.sandersme.advent.twentythree.model

import scala.annotation.tailrec

case class OasisReport(history: Vector[Vector[Int]]) {
  def calculateSumOfNext: Int = {
    history.map(OasisReport.calculateNextNumberInSequence)
      .sum
  }

  def calculateSumOfPrevious: Int = {
    history.map(OasisReport.calculatPreviousNumberInSequence)
      .sum
  }
}

object OasisReport {
  def parseInput(input: List[String]): OasisReport = {
    val parsedVectors = input.map(_.split(" ").map(_.toInt).toVector).toVector
    OasisReport(parsedVectors)
  }
  

  /**
    * The first thing we need to do is create the layers. Then from right to left we need
    * to take the last value of the bottom layer and add it to the last value of the next
    * layer up. We do this until we we reach the top. 
    *
    * @param sequence
    * @return
    */
  def calculateNextNumberInSequence(sequence: Vector[Int]): Int = {
    val layers = calculateLayersForLine(sequence)
    layers.map(_.last)
      .reduceRight(_ + _)
  }

  /**
    * First we need to create the layers then we take the first value of each
    * sequence.Then reduce from the left by subtracting the first value minus the result
    * in the layer.  
    *
    * @param sequence
    * @return
    */
  def calculatPreviousNumberInSequence(sequence: Vector[Int]): Int = {
    val layers = calculateLayersForLine(sequence)
    layers.map(_.head)
      .reduceRight(_ - _)
  }

  /**
  * For any given sequence we need to recursively create layers until we find all zeroes.
  * This is used to take the last value of each vector to find the next item in a sequence
  * of numbers
  *
  * @param sequence
  * @return
  */
  def calculateLayersForLine(sequence: Vector[Int]): Vector[Vector[Int]] = {
    @tailrec
    def loop(nextSequence: Vector[Int], results: Vector[Vector[Int]]): Vector[Vector[Int]] = {
      if (nextSequence.forall(_ == 0)) {
        results
      } else {
        val nextLayerDifference = calculateDifference(nextSequence)
        loop(nextLayerDifference, results :+ nextLayerDifference)
      }
    }

    loop(sequence, Vector(sequence))
  }
  
  /**
    * Loop through the sequence from right to left and subtract the right pointer from the
    * element one step to the left. THis should create a new sequence that contians the
    * difference of values. I might want to do this in the opposite direction 
    *
    * @param sequence
    * @return
    */
  def calculateDifference(sequence: Vector[Int]): Vector[Int] = {
    val lastIdx = sequence.size - 2
    val results = for {
      rightPtr <- 0 to lastIdx
    } yield sequence(rightPtr + 1) - sequence(rightPtr)
    results.toVector
  }
}
