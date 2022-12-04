package com.sandersme.advent.twentytwo.model

case class CalorieAccumulator(max: Long, currentAccumulation: Long)

object CalorieAccumulator {
  def empty: CalorieAccumulator = CalorieAccumulator(0, 0)

  def maxFromInputSequence(lines: Seq[String]): Long = {
    val calorieAccumulator = lines.foldLeft(CalorieAccumulator.empty){ (accum, nextVal) =>
      if (nextVal.trim.isBlank)
        val updatedMax = Math.max(accum.currentAccumulation, accum.max)
        CalorieAccumulator(updatedMax, 0)
      else
        val updatedAccumulation = accum.currentAccumulation + nextVal.toLong
        CalorieAccumulator(accum.max, updatedAccumulation)
    }

    calorieAccumulator.max
  }
  
  
}
