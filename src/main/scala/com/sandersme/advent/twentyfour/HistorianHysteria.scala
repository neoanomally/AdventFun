package com.sandersme.advent.twentyfour

import com.sandersme.advent.Input
import scala.collection.mutable.ListBuffer
import scala.annotation.tailrec

object HistorianHysteria {

  def main(args: Array[String]): Unit = {
    val input = Input.readTwentyFourFromResource("day1_input") 

    val (left, right) = createLists(input) 


    val total = compareLists(left, right, 0, 0)

    val similarityScore = calculateSimilarityScore(left, right)
  
    println(f"The total is $total")
    println(f"The similarity score is $similarityScore")
  }

  def createLists(input: List[String]): (List[Int], List[Int]) = {
    val left = new ListBuffer[Int]()
    val right = new ListBuffer[Int]()

    input.foreach{ case line => 
      val split = line.split("   ")
      left += split(0).toInt
      right += split(1).toInt
    }
    
    (left.toList.sorted, right.toList.sorted)
  }

  @tailrec
  def compareLists(left: List[Int], right: List[Int], i: Int, total: Int): Int = {
    if (i >= left.length) {
      total
    } else {
      val diff = Math.abs(left(i) - right(i))

      compareLists(left, right, i + 1, total + diff)
    }
  }

  def calculateSimilarityScore(left: List[Int], right: List[Int]): Int = {
    val rightCount: Map[Int, Int] = right.groupMapReduce(identity)(_ => 1)(_ + _)

    left.foldLeft(0){case (total, key) =>
      val product = key * rightCount.getOrElse(key, 0)

      total + product
    }
  }
}
