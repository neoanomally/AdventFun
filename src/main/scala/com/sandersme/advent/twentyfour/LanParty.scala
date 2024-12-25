package com.sandersme.advent.twentyfour

import com.sandersme.advent.Input

case class LanParty(networkGraph: Map[String, Set[String]]) { 
  def findThreesStartT: Set[Set[String]] = {
    LanParty.findEveryThreePair(networkGraph)
      .filter(_.exists(_.startsWith("t")))
  }


  /// Let's think about the algorithm: 
  // 1. At my node I want to grab the set of all candidates. Then I want to visit each of
  //    those neighbors and find the intersection of each of those candidates. 
  def findLargestInterConnectedNetwork: List[String] = {
    val interconnectedResults = networkGraph.keySet.foldLeft((Set.empty[Set[String]], Set.empty[String])) { case ((results, visited), current) =>
      val filteredCandidates = networkGraph(current)

      val finalCandidates = LanParty.traverseCurrent(current, filteredCandidates, networkGraph) 
      val updatedVisited = visited + current
      val updatedResults = if (finalCandidates.size == 1) results else results ++ finalCandidates
      
      (updatedResults, updatedVisited)
    }._1


    interconnectedResults
      .maxBy(_.size)
      .toList
      .sorted
  }
}

object LanParty {
  def main(args: Array[String]): Unit = {
    val input = Input.readTwentyFourFromResource("day23_input")

    val startTime = System.currentTimeMillis()
    val lanParty = LanParty.parseInput(input)

    val countNumThreePairs = lanParty.findThreesStartT
    println("Completed part one")
    
    val largestInterConnectedNetwork = lanParty.findLargestInterConnectedNetwork
    val totalTime = System.currentTimeMillis() - startTime

    println("The number of three sequence networks that start with t are: " + countNumThreePairs.size + " It took " + totalTime + "ms")
    println("PART Two largest interconnected: " + largestInterConnectedNetwork.mkString(","))
  }

  def findEveryThreePair(networkGraph: Map[String, Set[String]]): Set[Set[String]] = {
    // I should keep track of nodes that I've already visited. Because I'm searching for
    // sets of three I only need to hop one neighbor and verify. WHat i can do is at my
    // current node. For each pair, visit the other pair and see if any of my other
    // neighbors are in that list. 
    def exploreCurrent(current: String, visited: Set[String]): Set[Set[String]] = {
      val neighbors = networkGraph(current).filter(!visited.contains(_))
       
      // NEED TO FOLD LEFT Because we don't want to keep hopping around 
      neighbors.foldLeft((Set.empty[Set[String]], visited)) { case ((threeSets, visitedWithCurrent), neighbor) => 
        if (networkGraph.contains(neighbor)) {
          val thirdPairs = exploreNeighbor(neighbor, neighbors, visited)
          val updatedThreeSets = threeSets ++ thirdPairs.map(third => Set(current, neighbor, third))
          val updatedVisited = visited + neighbor
          
          (updatedThreeSets, updatedVisited)
        } else {
          (threeSets, visited + neighbor)
        }
      }._1
    }

    def exploreNeighbor(neighbor: String, candidates: Set[String], visited: Set[String]): Set[String] = {
      networkGraph(neighbor)
        .filter(!visited.contains(_))
        .filter(candidates.contains)
    }

    networkGraph.keySet.foldLeft(Set.empty[Set[String]], Set.empty[String]){ case ((results, visited), node) =>
      val updatedVisited = visited + node
      val updatedResults = results ++ exploreCurrent(node, updatedVisited)

      (updatedResults, updatedVisited)
    }._1
  }
    /// FIlter out any candidate that is not a neighbor. If we do this every time we
    // should end up with the intersection where all neighbros are neighbors of each other. 
  def filterNeighborCandidates(neighbor:String, candidates: Set[String], neighborNeighbors: Set[String]): Set[String] = {
    val candidatesWithNeighbor = neighborNeighbors + neighbor
    val res = candidates.filter(candidate => candidatesWithNeighbor.contains(candidate))
    res
  }

  def traverseCurrent(current: String, candidates: Set[String], networkGraph: Map[String, Set[String]]): Set[Set[String]] = {
    val candidatesMap = Map.empty[Set[String], Int]
    val candidatessWithNCurrent = candidates + current

    val finalCandidates = candidates.foldLeft(candidatesMap){ case (mapAgg, neighbor) => 
      val neighborCandidates = filterNeighborCandidates(neighbor, candidatessWithNCurrent, networkGraph(neighbor))
      val updatedValue = mapAgg.getOrElse(neighborCandidates, 1) + 1
      mapAgg + (neighborCandidates -> updatedValue)
    }

    finalCandidates.filter((k, v) => k.size == v)
      .keySet
  }

  def parseInput(in: List[String]): LanParty = {
    val networkGraph = in.map{line => 
      val split = line.split("-")

      val left = split(0)
      val right = split(1)
      (left, right)
    }.foldLeft(Map.empty[String, Set[String]]){ case (graph, (left, right)) =>
      val leftEntry = graph.getOrElse(left, Set.empty) + right
      val rightEntry = graph.getOrElse(right, Set.empty) + left

      graph + (left -> leftEntry, right -> rightEntry)
    }

    LanParty(networkGraph)
  }
}
