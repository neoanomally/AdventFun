package com.sandersme.advent.twentyfour

import scala.annotation.tailrec
import com.sandersme.advent.Input

case class CodeChronicle(locks: Vector[Vector[Int]], keys: Vector[Vector[Int]]) {
  def addLocks(lock: Vector[Int]): CodeChronicle = {
    this.copy(locks = locks :+ lock)
  }

  def addKeys(key: Vector[Int]): CodeChronicle = {
    this.copy(keys = keys :+ key)
  }

  def countNumKeyLockFits: Int = {
    locks.foldLeft(0){ case (lockSum, lock) => 
      keys.foldLeft(lockSum){ case (keySum, key) =>
        val res = if (CodeChronicle.lockFits(lock, key)) 1 else 0

        keySum + res
      }
    }
  }
}

object CodeChronicle {
  def main(args: Array[String]): Unit = {
    val input = Input.readTwentyFourFromResource("day25_input")
    val startTime = System.currentTimeMillis()
    val codeChronicle = CodeChronicle.parseInput(input)

    val combinations = codeChronicle.countNumKeyLockFits
    val endTime = System.currentTimeMillis() - startTime

    println(f"The number of lock-key combinations is ${combinations}, Took ${endTime}ms")
  }

  def lockFits(left: Vector[Int], right: Vector[Int]): Boolean = {
    (0 until 5).forall(idx => (left(idx) + right(idx)) < 6)
  }

  def empty: CodeChronicle = {
    CodeChronicle(Vector.empty, Vector.empty)
  }

  @tailrec
  def parseInput(in: List[String], results: CodeChronicle = CodeChronicle.empty): CodeChronicle = {
    if (in.size < 3) {
      results
    } else {
      val next = in.dropWhile(_.isEmpty).take(7)
      val remaining = in.dropWhile(_.isEmpty).drop(7)

      val nextCandidate = next.zipWithIndex.foldLeft(Vector(-1, -1, -1, -1, -1)){ case (yAgg, (line, y)) =>
        line.zipWithIndex.foldLeft(yAgg){ case (xAgg, (value, x)) => 
          val res = if (value == '#') 1 else 0

          xAgg.updated(x, xAgg(x) + res)
        }
      }

      val updatedResults = if (next.head == LOCK_TOP && next.last == LOCK_BOTOTM) {
        results.addLocks(nextCandidate)
      } else {
        results.addKeys(nextCandidate)
      }

      parseInput(remaining, updatedResults)
    }

  }

  private val LOCK_TOP = "#####"
  private val LOCK_BOTOTM  = "....."
}
