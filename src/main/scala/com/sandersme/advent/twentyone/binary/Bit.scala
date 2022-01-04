package com.sandersme.advent.twentyone.binary

sealed trait Bit
object One extends Bit {
  override def toString: String = "1"
}
object Zero extends Bit {
  override def toString: String = "0"
}
