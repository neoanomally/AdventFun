package com.sandersme.advent.twentyfour

import com.sandersme.advent.twentyone.model.Cuboid.intersection


case class KeyGraph(valueLookup: Map[Char, Point], pointLookup: Map[Point, Char], height: Int, width: Int)

case class KeypadConundrum(keypadGraph: KeyGraph, directionGraph: KeyGraph, instructions: List[List[Char]]) {

}
case class KeyPath(value: Char, direction: Char, path: List[Char]) {
  
}

object KeypadConundrum {
  def main(args: Array[String]): Unit = {
    println("hi")
  }

  def createKeyPoints(graph: Vector[Vector[Char]]): KeyGraph = {
    val graphPosSeq = for { 
      y <- 0 until graph.length
      x <- 0 until graph(0).length

      if (graph(y)(x) != '-')
    } yield graph(y)(x) -> Point(x, y)

    KeyGraph(graphPosSeq.toMap, graphPosSeq.map(_.swap).toMap, graph.length, graph(0).length)
  }


  def calculateShortestPaths(graph: KeyGraph): Map[Char, Map[Char,  List[List[Char]]]] = {
    graph.valueLookup
      .foldLeft(Map.empty[Char, Map[Char, List[List[Char]]]]){ case (res, (value, _)) =>
        
        val keyPath = KeyPath(value, '-', List.empty)

        val shortestPaths = calculateShortestPath(List(keyPath), graph, Map.empty)
        
        res + (value -> shortestPaths) 
      }
  }

  /// For the keypath it might be better just rto return the value
  // TODO I should probably have both the value that it goes over as well 
  // The different direction path; however the direction path can be used
  // to calculate across ROBOTs
  def calculateShortestPath(queue: List[KeyPath], keyGraph: KeyGraph,
    res: Map[Char, List[List[Char]]]): Map[Char, List[List[Char]]] = {

    if (queue.isEmpty) {
      res
    } else {
      val updatedQueue = queue.flatMap{ keyPath =>
        findGridNeighbors(keyPath, keyGraph)
          .filter(neighborKeypath => !res.contains(neighborKeypath.value))
      }

      val updatedMap = updatedQueue.groupBy(keyPath => keyPath.value)
        .map((k, v) => k -> v.map(_.path))
      
      val updatedRes = res ++ updatedMap
      calculateShortestPath(updatedQueue, keyGraph, updatedRes)
    }
  }

  /// TODO Instead of points for the keypath lookup might be better just to return the
  //value
  def findGridNeighbors(keyPath: KeyPath, keyGraph: KeyGraph): List[KeyPath] = {
    val point = keyGraph.valueLookup(keyPath.value)
    val updatedPath = keyPath.path :+ keyPath.direction
    List(
      (Point(point.x + 1,point.y), '>'),
      (Point(point.x - 1, point.y), '<'),
      (Point(point.x, point.y + 1), 'v'),
      (Point(point.x, point.y - 1), '^')
    ).filter((p, _) => p.x >= 0 && p.y >= 0 && p.x < keyGraph.width && p.y < keyGraph.height)
      .filter((p, _)  => keyGraph.pointLookup.contains(p))
      .map((point, dir) => KeyPath(keyGraph.pointLookup(point), dir, updatedPath))
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
