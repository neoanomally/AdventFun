package com.sandersme.advent.graph

import com.sandersme.advent.graph.{ Node, Point}

trait Node[A] {
  val value: A
  val neighbors: List[Point]
}

case class GridNode(value: Int, neighbors: List[Point], location: Point) extends Node[Int] {
  override def toString: String = s"Value: $value   neighbors: $neighbors   location: $location"
}