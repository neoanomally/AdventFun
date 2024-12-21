package com.sandersme.advent.twentyfour

import com.sandersme.advent.twentyone.model.Cuboid.intersection


case class KeypadConundrum(keypadGraph: Map[Char, Point], directionGraph: Map[Char, Point], instructions: List[List[Char]])

object KeypadConundrum {
  def main(args: Array[String]): Unit = {
    println("hi")
  }

  def createKeyPoints(graph: Vector[Vector[Char]]): Map[Char, Point] = {
    val graphPosSeq = for { 
      y <- 0 until graph.length
      x <- 0 until graph(0).length

      if (graph(y)(x) != '-')
    } yield graph(y)(x) -> Point(x, y)

    graphPosSeq.toMap
  }


  def calculateShortestPath(): Map[Int, List[Char]] = {
    ???
  }


  def parseInput(in: List[String]): KeypadConundrum = {
    val instructions = in.map(_.toCharArray.toList)

    val numericpad = createKeyPoints(NumericKeypadGraph)
    val directionalpad = createKeyPoints(DirectionalKeyPad)

    KeypadConundrum(numericpad, directionalpad, instructions)
  }
}

val NumericKeypadGraph = Vector(
  Vector('7', '8', '9'),
  Vector('4', '5', '6'),
  Vector('1', '2', '3'),
  Vector('-', '0', 'A')
)

val DirectionalKeyPad = Vector(
  Vector('-', '^', 'A'),
  Vector('<', 'v', '>')
)
