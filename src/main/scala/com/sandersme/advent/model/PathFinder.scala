package com.sandersme.advent.model

import com.sandersme.advent.model.PathFinder.{StartNode, *}


type PathMap = Map[PathNode, List[PathNode]]

object PathFinder {
  // TODO I don't need the smallCavesVisited I could just have a collect statement on path,
  // TODO Build a memoization map that takes a root node and all possible nodes.
  // Then all we have to do is for the next node append all future routes,
  // One of the things we have to do then though is filter out routes based on the multiple
  // Small cave criterion.
  // but that's a performance consideration with trade offs
  case class Route(nextNode: PathNode = StartNode,
                   path: List[PathNode] = List.empty)  {
    def hasNoConnections(pathMap: Map[PathNode, List[PathNode]]): Boolean =
      !pathMap.contains(nextNode)

    def isNotVisitableSmallCave: Boolean = isSmallCaveAlreadyVisited && hasVisitedASmallCavesTwice

    def isVisitableSmallCave: Boolean = {
      !isSmallCaveAlreadyVisited || hasVisitedSmallCavesOnce
    }

    def isSmallCaveAlreadyVisited: Boolean = {
      nextNode.isInstanceOf[SmallCave] && path.contains(nextNode)
    }

    def hasVisitedASmallCavesTwice: Boolean = !hasVisitedSmallCavesOnce

    def hasVisitedSmallCavesOnce: Boolean = {
      path.filter(_.isInstanceOf[SmallCave])
        .groupBy(identity)
        .forall(_._2.size == 1)
    }

    def isSmallCave: Boolean = nextNode.isInstanceOf[SmallCave]
    def isStartNodeAfterRunning: Boolean = nextNode == StartNode && path.nonEmpty
    def isEndNode: Boolean = nextNode == EndNode
    def hasEndNode: Boolean = path.last == EndNode

    def shouldStopPathFinding: Boolean = {
      isNotVisitableSmallCave || isEndNode || isStartNodeAfterRunning
    }
  }
  // One way to do this is have a map of each name that goes to a list of paths.
  // Then when we want to traverse, we go to the start
  sealed trait PathNode
  case class SmallCave(name: String) extends PathNode
  case class LargeCave(name: String) extends PathNode
  case object StartNode extends PathNode
  case object EndNode extends PathNode

  def parseInput(input: List[String]): PathMap = {
    val parsedInput = input.map { line =>
      val split = line.split("-")
      parsePathNode(split(0)) -> parsePathNode(split(1))
    }

    // Swap removing any back connections back to start node since it's unvisitable
    val swappedParsedInput = parsedInput.map(_.swap)

    val pathMap = (parsedInput ++ swappedParsedInput)
      .groupBy(_._1)
      .map{ case(node, edges) => node -> edges.map(_._2)}

    pathMap
  }

  private[model] def parsePathNode(name: String): PathNode = {
    name match {
      case "start" => StartNode
      case "end" => EndNode
      case u if name(0).isUpper => LargeCave(name)
      case _ => SmallCave(name)
    }
  }

  /**
   * What I'm going to do is at each step, build append to the list of routes from their neighbors.
   * If there are no neighbors end
   */
  private[model] def recursiveFindPaths: List[Route] = {
    ???
  }


  // TODO: A better way to do this is instead add all neighbors to places to visit
  // Then call ourself. The problem with this approach is that it's not tail recursive because
  // we are doing a flatmap then doing findAllPaths. This is not good. To have tail recursion
  // the last call in the function needs to be to itself.
  def findPathsToEnd(pathMap: PathMap, route: Route = Route()): List[Route] = {
    val updatedPath = route.path :+ route.nextNode

    if (route.shouldStopPathFinding) {
      // aggressively filter out end nodes optimization
      List(route.copy(path = updatedPath)).filter(_.path.last == EndNode)
    } else {
      val nextRoute = route.copy(path = updatedPath)

      pathMap(route.nextNode)
        .flatMap(nextNode => findPathsToEnd(pathMap, nextRoute.copy(nextNode = nextNode)))
    }
  }

}
