package com.sandersme.advent.twentyfour

import com.sandersme.advent.twentyfour.PathGraph
import com.sandersme.advent.Input
import scala.collection.mutable.{Map => MMap}
import scala.annotation.tailrec


type PathGraph = Map[Char, Map[Char, List[List[Char]]]]

case class KeyGraph(valueLookup: Map[Char, Point], pointLookup: Map[Point, Char], height: Int, width: Int) {
  
}

case class KeypadConundrum(keypadGraph: KeyGraph, directionGraph: KeyGraph, keyPaths: PathGraph, directionPaths: PathGraph, instructions: List[String], numRobots: Int) { 
  def calculateShortestPathCosts: Long = {
    instructions
      .map(inst => inst.dropRight(1).toLong * calculateShortestPath(inst))
      .sum
  }

  def calculateShortestPath(combination: String): Int = {
    calculateSecondPath(combination)
      .flatMap(in => calculatePath(in, directionPaths, MMap.empty))
      .map(_.size)
      .min
  }

  def calculateSecondPath(combination: String): List[List[Char]] = {
    calculateFirstPath(combination)
      .flatMap(in =>  calculatePath(in, directionPaths, MMap.empty))
  }



  def calculateFirstPath(combination: String): List[List[Char]] = {
    calculatePath(combination.toList, keyPaths, MMap.empty)
  }

  def calculateCostPartTwo: Long = {
    
    instructions
      .map{ combination => 
        val cache = MMap.empty[(Char, Char, Int), Long]
        val cost = calculatePathPartTwo(combination.toList, cache, 0)
        val multiple = combination.dropRight(1).toLong
        cost * multiple
      }
      .sum
  }

  // DO I NEED TO DO SOMETHING SPECIFIC FOR THE 25th value?
  def calculatePathPartTwo(combination: List[Char], cache: MMap[(Char, Char, Int), Long], stage: Int): Long = {
    val keypad = if (stage > 0) directionPaths else keyPaths

    combination.foldLeft(('A', 0L)){ case ((start, sum), end) =>
      val shortestDist = cache.getOrElseUpdate((start, end, stage), {
        shortestCurrentPath(start, end, stage, cache)
      })

      (end, sum +  shortestDist)
    }._2
  }

  /// So I should start with the first value so in the case of 029A
  // We'll first hit 0, and then for that we'll find the shortest path for the first robot
  // to hit zero.
  // Once there the next robot will find the shortest path to get the robot to zero from
  // A
  // The next robot will take the list of sequences starting from A -> next value
  def shortestCurrentPath(start: Char, end: Char, stage: Int, cache: MMap[(Char, Char, Int), Long]): Long = {
    val graph = if (stage == 0) { keyPaths } else { directionPaths }
    
    if (start == end) {
      1L
    } else {

    graph(start)(end).map{ case combination => 
        if (stage < numRobots) {
          val dist = calculatePathPartTwo(combination :+ 'A', cache, stage + 1)
          dist
        } else {
          combination.size.toLong + 1L // The 1L is to acccount for clicking A
        }
      }.min
    }
  }

  def findShortestCombinations(start: Char, end: Char, stage: Int): List[List[Char]] = {
    val keypad = if (stage > 0) directionPaths else keyPaths

    // println("CALCULATING SHORTEST COMBO STAGE: " + stage + f" START: $start  END: $end ")
    keypad(start)(end).map(_ :+ 'A')
  }

  // Knowing this will help calculate the second path. 
  def calculatePath(combination: List[Char], graph: PathGraph, cache: MMap[(Char, Char, Int), Long]): List[List[Char]] = {
    val (lastLoc, shortestPath) = combination.foldLeft(('A', List.empty[List[Char]])) { case ((startLoc, sum), nextLoc) => 

      val distances = if (startLoc == nextLoc) {
        List(List('A'))
      } else {
        val graphDist = graph(startLoc)(nextLoc).map(_ :+ 'A')
        val minSizes = graphDist.map(_.size).min
        graphDist.filter(_.size == minSizes)
      }

      val f = if (sum.isEmpty) {
        distances
      } else { 
        sum.flatMap(sofar => distances.map(dist => sofar ++ dist))
      }
      (nextLoc, f)
    }

    shortestPath
  }
}


case class KeyPath(value: Char, direction: Char, path: List[Char]) {
  def createEnd: KeyPath = {
    // maybe this should be char
    this.copy(path = path.tail :+ direction)
  }
}


/**
  * One directional keypad that you are using.
    Two directional keypads that robots are using.
    One numeric keypad (on a door) that a robot is using.

    WE can see the path for just the numeric keypad that the final 
    robot should do. Make sure that path works
  
    Then go up etc. 
  */
object KeypadConundrum {
  def main(args: Array[String]): Unit = {
    val input = Input.readTwentyFourFromResource("day21_input")

    val keypadConundrum = KeypadConundrum.parseInput(input)
    // val shortestPathCost = keypadConundrum.calculateShortestPathCosts
    //
    // println(f"The cost for the shortest path is: $shortestPathCost")
    val shortestPathCostPartOne = keypadConundrum.copy(numRobots = 2).calculateCostPartTwo

    println(f"The cost for the shortest path in part one is $shortestPathCostPartOne and should be 179444")

    val partTwoCost = keypadConundrum.calculateCostPartTwo
    println(keypadConundrum.numRobots)
    println(f"The cost for shortest Path for 25 robots is: $partTwoCost")
  }

  def createKeyPoints(graph: Vector[Vector[Char]]): KeyGraph = {
    val graphPosSeq = for { 
      y <- 0 until graph.length
      x <- 0 until graph(0).length

      if (graph(y)(x) != '-')
    } yield graph(y)(x) -> Point(x, y)

    KeyGraph(graphPosSeq.toMap, graphPosSeq.map(_.swap).toMap, graph.length, graph(0).length)
  }


  def calculateShortestPaths(graph: KeyGraph): Map[Char, Map[Char, List[List[Char]]]]= {
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

      val updatedMap = updatedQueue.map(keyPath => keyPath.copy(path = keyPath.path.tail :+ keyPath.direction))
        .groupBy(keyPath => keyPath.value)
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
    val instructions = in

    val numericpad = createKeyPoints(NumericKeypadGraph)
    val directionalpad = createKeyPoints(DirectionalKeyPad)
  
    val numericPaths = calculateShortestPaths(numericpad)
    val directionPaths = calculateShortestPaths(directionalpad)

    KeypadConundrum(numericpad, directionalpad, numericPaths, directionPaths, instructions, 25)
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
