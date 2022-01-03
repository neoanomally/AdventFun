package com.sandersme.advent.model

case class MinMaxDimensions(minX: Int, minY: Int, maxX: Int, maxY: Int)

object MinMaxDimensions {
  def default: MinMaxDimensions = {
    MinMaxDimensions(Integer.MAX_VALUE, Integer.MAX_VALUE,
      Integer.MIN_VALUE, Integer.MIN_VALUE)
  }
}