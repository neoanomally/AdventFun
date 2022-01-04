package com.sandersme.advent.twentyone.model

case class PolyPair(left: Char, right: Char) {
  override def toString: String = s"$left$right"
}