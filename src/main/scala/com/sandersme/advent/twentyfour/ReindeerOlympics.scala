package com.sandersme.advent.twentyfour
import RObj.* 
import Direction.*
import com.sandersme.advent.Input
import scala.collection.mutable.PriorityQueue
import scala.collection.mutable.{Set => MSet }
import scala.collection.mutable.{Map => MMap }
import com.sandersme.advent.twentyfour.Direction
import com.sandersme.advent.twentyfour.ReindeerOlympics.isOneStep
import com.sandersme.advent.twentyfour.NodeOrder
import scala.annotation.tailrec
import com.sandersme.advent.twentytwo.model.PacketType.startParsing
import com.sandersme.advent.twentyfour.Direction
import com.sandersme.advent.twentytwo.model.Coord.stepFromDirection
import scala.quoted.runtime.Patterns.fromAbove

enum RObj {
  case Start, End, RWall, ROpen
}

case class RNode(p: Point, score: Long, facing: Direction, steps: Int, prevPoints: List[Point]) {
  def addPrevPoint(newPoint: Point): RNode = {
    this.copy(prevPoints = prevPoints :+ newPoint)
  }

  def addPrevPoints(newPoints: List[Point]): RNode = {
    this.copy(prevPoints = prevPoints ++ newPoints, steps = steps + 1)
  }

  def incrSteps: RNode = {
    this.copy(steps = steps + 1)
  }
}

object NodeOrder extends Ordering[RNode] {
  def compare(x: RNode, y: RNode): Int = y.score.compare( x.score)
}

case class ReindeerOlympics(grid: Vector[Vector[RObj]], reindeerLoc: Point, facing: Direction, endLoc: Point) {

  val height: Int = grid.length
  val width: Int = grid(0).length

  def objectAt(p: Point): RObj = {
    objectAt(p.x, p.y)
  }

  def objectAt(x: Int, y: Int): RObj = {
    grid(y)(x)
  } 

  case class FiveNode(point: Point, score: Int, dir: Direction) {
    def combineScore(other: FiveNode): FiveNode = {
      val dirScoreDifference = if (this.dir == other.dir) {
        0
      } else if (isOneStep(this.dir, other.dir)) {
        1000
      } else { 
        2000
      }

      val mod = if (this.point == endLoc || other.point == endLoc || this.point== reindeerLoc || other.point == reindeerLoc) {
        0 
      } else { 
        -2000
      }

      this.copy(score = score + other.score + dirScoreDifference + mod)
    }
  }
  case object FiveNodeOrder extends Ordering[FiveNode] {
    def compare(x: FiveNode, y: FiveNode): Int = y.score.compare(x.score)
  }
  def startTraverseFive: List[FiveNode] = {
    val startNodePq = new PriorityQueue[FiveNode]()(FiveNodeOrder)
    val endNodePq = new PriorityQueue[FiveNode]()(FiveNodeOrder) 
    findValidNeighbors(endLoc)
      .foreach { case (point, dir) =>
        endNodePq.enqueue(FiveNode(point, 1001, dir)) // maybe this starts as a 1000?
      }
    
    findValidNeighbors(reindeerLoc)
      .foreach { case (point, dir) => 
        startNodePq.enqueue(FiveNode(point, 1001, dir))
      }

    val traverseFromEnd = traverseFive(endNodePq,  Map(endLoc -> FiveNode(endLoc, 0,  Up)))
    val traverseFromStart = traverseFive(startNodePq, Map(reindeerLoc -> FiveNode(reindeerLoc, 0, Up)))

    traverseFromStart
      .map((key, fivePoint) => fivePoint.combineScore(traverseFromEnd(key)))
      .toList
  }

  @tailrec
  final def traverseFive(pq: PriorityQueue[FiveNode], res: Map[Point, FiveNode]): Map[Point, FiveNode] = {
    if (pq.isEmpty) {
      res
    } else if (res.contains(pq.head.point)) {
      pq.dequeue
      traverseFive(pq, res)
    } else {
      val next = pq.dequeue 
      
      findValidNeighbors(next.point)
        .filter(v => !res.contains(v._1)) 
        .foreach{ case (point, dir) => 
          val cost = calculateCostDiff(dir, next.dir, next.score)
          pq.enqueue(FiveNode(point, cost, dir))
        }

      traverseFive(pq, res + (next.point -> next))
    }
  }


  def calculateCostDiff(a: Direction, b: Direction, score: Int): Int = {
    if (a == b) {
      score + 1
    } else if (isOneStep(a, b)) {
      score + 1001
    } else {
      score + 2001
    }

  }
  def subtractCostDiff(a: Direction, b: Direction, score: Int): Int = {
    if (a == b) {
      score
    } else if (isOneStep(a, b)) {
      score - 999
    } else {
      score - 1999
    }

  }
  
  // Gonna try the same technique as the first one. I want to use a PriorityQueue, and
  // with the queue I want to keep track of the prevNode and current node. 
  // Those will be kept track within a Map. We'll include a map of Point -> Data Container 
  // that has a pair of prevNode and Score. WE'll also keep track of a global
  // Score because we'll want to pop things off the queue until the pq has a score > than
  // the endScore which will stay Int.MaxValue until 
  // The idea is that we can then create a path

  case class NodeScore(point: Point, prev: Point, dir: Direction, score: Int) { }
  case object NodeScoreOrder extends Ordering[NodeScore] {
    def compare(x: NodeScore, y: NodeScore): Int = y.score.compare(x.score)
  }


  def combineMaps(left: Map[Point, Int], right: Map[Point, Int]): Map[Point, Int] = {
  
      val allKeys = left.keySet ++ right.keySet
      
      allKeys.map { key => 
        val lowestScore = left.get(key)
          .map(keyScore => Math.min(keyScore, right.getOrElse(key, Int.MaxValue)))
          .getOrElse(right(key)) // This should not fail. keeping it because I want to know if i get here
        key -> lowestScore
      }.toMap 
  }


  /// MAYBE instead of using a Set for Results we have a Map[Point, Score] -- SCore at
  //that point such that if another point gets to this point and the score <= score then
  //the visited will be added to the results if not just return the previous result
  def endScoreDFS(current: Point, dir: Direction, score: Int, finalScore: Int, visited: MMap[Point, Int], onlyVisitTheseNodes: Set[Point], res: Map[Point, Int]): Map[Point, Int] = {
    if (score > finalScore) {
      Map.empty
    } else if (current == endLoc) {
      combineMaps(visited.toMap, res + (current -> score))
    } else if (res.contains(current) && res(current) <= score) {
      combineMaps(visited.toMap, res)
    } else if (res.contains(current) && res(current) > score) {
      Map.empty
    } else {
      findValidNeighbors(current)
        .filter((n, _) => !visited.contains(n))
        .filter((n, _) => onlyVisitTheseNodes.contains(n))
        .map { case (neighbor, neighborDir) => 
          val updatedCost = calculateCostDiff(dir, neighborDir, score)
          visited += (current -> score)
          val dfsResults = endScoreDFS(neighbor, neighborDir, updatedCost, finalScore, visited, onlyVisitTheseNodes, res)
          visited -= current
          dfsResults
        }.foldLeft(Map.empty[Point, Int])((agg, next) => combineMaps(agg, next))
    }
  }

  def startGridSix: (Map[Point, EndScore], Set[Point]) = {
    val pq = new PriorityQueue[NodeScore]()(NodeScoreOrder)
    pq.enqueue(NodeScore(reindeerLoc, reindeerLoc, Right, 0))
    println("ABOUT TO START")
    val map = traverseGridSix(pq, Map.empty, endLoc)

    // for each entry see if we can get to the end using the direction 
    // where we go to the next node in the direction from the previous
    //
    // map.toList.sortWith((l, r) => if (l._1.x == r._1.x) l._1.y < r._1.y else l._1.x < r._1.x)
    //   .foreach(println)
    // val onlyVisitTheseNodes = map.map(_._1).toSet
    // val finalSet = endScoreDFS(reindeerLoc, Right, 0, map(endLoc).score, MMap.empty, onlyVisitTheseNodes, Map.empty)
    // (map, finalSet.keySet)
    val finalSet = highlyCachedCompute(map)
    (map, finalSet)
  }

  def highlyCachedCompute(map: Map[Point, EndScore]): Set[Point] = {
    val finalScore = map(endLoc).score
  
    map.toList
      .sortWith{ (l, r) => l._2.score > r._2.score }
      .foldLeft((Set.empty[Point], Set.empty[Point])){ case ((isOnPath, isNotOnPath), (point, endScore)) => 

        print(f"Searching ${point} with endScore: $endScore and is on path: ")
        if (cachedDfsTwo(point, endScore, map, isOnPath, isNotOnPath)) {
          (isOnPath + point, isNotOnPath)
        } else {
          (isOnPath, isNotOnPath + point)
        }
      }._1
  }

  def cachedDfs(current: Point, endscore: EndScore , cache: Map[Point, EndScore], isOnPath: Set[Point], isNotOnPath: Set[Point]): Boolean = {
    if(current == endLoc) {
      true 
    } else { 
      val filteredNeighbors = findValidNeighbors(current)
        .filter(_._1 != endscore.prev) // don't go to the previous value
        .filter(p => cache.contains(p._1))
        .filter(p => !isNotOnPath.contains(p._1))

        filteredNeighbors.exists{case (neighbor, neighborDir) => 
          calculateCostDiff(neighborDir, endscore.dir, endscore.score) ==  cache(neighbor).score
        }
      }
    }

  def cachedDfsTwo(current: Point, endscore: EndScore , cache: Map[Point, EndScore], isOnPath: Set[Point], isNotOnPath: Set[Point]): Boolean = {
    if(current == endLoc) {
      true 
    } else { 
      val filteredNeighbors = findValidNeighbors(current)
        .filter(_._1 != endscore.prev) // don't go to the previous value
        .filter(p => cache.contains(p._1))
        .filter(p => !isNotOnPath.contains(p._1))


      if (current == Point(15, 8)) {
        println("Filtered neighbors for Point(15, 8): should  have 15, 7" + filteredNeighbors)
      }

      val isConnected = filteredNeighbors.exists{case (neighbor, neighborDir) => 
        subtractCostDiff(neighborDir, endscore.dir, endscore.score) ==  cache(neighbor).score
      }
      
      if (isConnected) {
        true
      } else {
        val neighborsNotVisited = filteredNeighbors.filter((point, dir) => !isOnPath(point) && !isNotOnPath(point))
       
        neighborsNotVisited.exists{ (point, dir) => 
          val cachedNeighbor = cache(point)
          // might need to rotate both; because I needd the rotation and so maybe I also
          // return the rotation? 
          val costDifference = subtractCostDiff(dir, endscore.dir, endscore.score) ==  cachedNeighbor.score
          val found = cachedDfs(point, cachedNeighbor, cache, isOnPath, isNotOnPath)

          if (Point(15, 8) == current) {
            val costDiff = subtractCostDiff(dir, endscore.dir, endscore.score)
              // println(f"visiting neighbor: ${point} (Cost difference: $costDifference and isFound: $found   current cost: ${endscore.score} neighbor Score ${cachedNeighbor.score} cost diff: ${costDiff}")
          }
          found && costDifference
        }
      }
    }
  }

  // FUCK, I probably need to  keep track of the direction that they came from 
  // that way when I add a new point the newScore is actually the difference
  // between those directions. Gonna try to hack it by making newScore <= score + 2000
  case class EndScore(score: Int, prev: Point, dir: Direction) {
    def toNodeScore(point: Point): NodeScore = {
      NodeScore(point, prev, dir, score) 
    }
  }
  
  def startTraverseSixDFSThree: Set[Point] = {
    val pq = new PriorityQueue[NodeScore]()(NodeScoreOrder)
    pq.enqueue(NodeScore(reindeerLoc, reindeerLoc, Right, 0))
  
    val map = traverseGridSix(pq, Map.empty, endLoc)

    dfsThree(map)
  }

  def dfsThree(map: Map[Point, EndScore]): Set[Point] = {
    val finalScore = map(endLoc).score

    println("DFS 3" )
    map.foldLeft(Set.empty[Point]){ case (aggSet, (point, endScore)) => 
      
      val pq = new PriorityQueue[NodeScore]()(NodeScoreOrder)
      pq.enqueue(NodeScore(point, endScore.prev, endScore.dir, endScore.score))
      val findEnd = traverseGridSix(pq, Map.empty, endLoc, finalScore)
      // findEnd.foreach(println)
      //
      // println("\n\n")
      
      if (findEnd.contains(endLoc)) {
        aggSet + point
      } else {
        aggSet
      }
    }
  }



  def constructHasSeen(current: Point, map: Map[Point, EndScore], res: Map[Point, EndScore]): Map[Point, EndScore] = {
    if (current == reindeerLoc) {
      res + (current -> map(current))  
    } else {
      val prev = map(current).prev
      constructHasSeen(prev, map, res + (current -> map(prev)))
    }
  }

  @tailrec
  final def traverseGridSix(pq: PriorityQueue[NodeScore], res: Map[Point, EndScore],
    endPoint: Point, endScore: Int = Int.MaxValue, debug: Boolean = false): Map[Point, EndScore] = {

    if (pq.isEmpty) {
      if (debug) { println("\t\tNothing more to search") }
      res
    } else if (pq.head.score > endScore) {
      if (debug) { println("\t" + f"the score: ${pq.head.score} is above ${endScore} for point ${pq.head.point} ") }
      res
    } else if(res.contains(pq.head.point)) {
      val next = pq.dequeue
      if (debug) { println(f"   Point ${next.point} exists as ${res(next.point)} -> replace -> $next}") }
      traverseGridSix(pq, res, endPoint, endScore)
    } else if (pq.head.point == endPoint) {
      val next = pq.dequeue
      val updatedMap = res + (next.point -> EndScore(next.score, next.prev, next.dir)) 
      val updatedScore = Math.min(endScore, next.score)
      traverseGridSix(pq, updatedMap, endPoint, updatedScore)
    } else {
      val next = pq.dequeue
      
      findValidNeighbors(next.point)
        .filter( (neighborPoint, dir) =>  neighborPoint != next.prev)
        .filter{(neighborPoint, dir) => !res.contains(neighborPoint)}
        .foreach { case (neighbor, dir) => 
          val cost = calculateCostDiff(next.dir, dir, next.score)
          if (debug) { println(f"   The cost: for neighbor ${neighbor} for ${next.point} is ${cost}") }
          pq.enqueue(NodeScore(neighbor, next.point, dir, cost))      
        }
      
      val updatedMap = res + (next.point -> EndScore(next.score, next.prev, next.dir))
      traverseGridSix(pq, updatedMap, endPoint, endScore)
    }
  }
  /// Let me think even more I have now tried several methods. I have tried using a
  //priority Queue 

  def findValidNeighbors(p: Point): List[(Point, Direction)] = {
    List(
      (Point(p.x + 1, p.y), Right),
      (Point(p.x - 1, p.y), Direction.Left),
      (Point(p.x, p.y - 1), Up),
      (Point(p.x, p.y + 1), Down)
    ).filter { case (point, _) => 
      point.x >= 0 && point.y >= 0 && point.y < height && point.x < width
    }.filter((point, dir) => objectAt(point) != RWall)
  }

  def calculateScore: Long = {
    startTraverse._1(endLoc).score
  }

  def calculateScoreTwo: Long = {
    startTraverse._1(endLoc).steps
  }

  def startTraverse: (Map[Point, RNode], Int) = { 
    val pq = ReindeerOlympics.emptyNodeOrderQueue
    pq.enqueue(RNode(reindeerLoc, 0, Right, 0, List(reindeerLoc)))

    traverseGrid(pq, Map.empty, 0 )
  }
/// Another way I can do this is by having a priority queue that keeps track of 
//  the next minimum cost to a node. I'll need to keep track of Point, score, direction
//  Then I add all unvisited neighbors to the queue with a point, score and facing.
//  The next iteration I pull off the queue and see if I'm at the end!
  @tailrec
  final def traverseGrid(pq: PriorityQueue[RNode], visited: Map[Point, RNode], totalSteps: Int): (Map[Point, RNode], Int) = {
    val next = pq.dequeue()
    // println("next: " + next)
    // println("Next Score: " + next.score)
    if (visited.contains(next.p)) {
      // println("Visited ALready contains point " + next + "\tvisited entry: " + visited(next.p))
      val updatedNodesPath = if (visited(next.p).score == next.score) {
        visited + (next.p -> visited(next.p).incrSteps)
      } else {
        visited
      }
      traverseGrid(pq, updatedNodesPath, totalSteps + 1)
    } else if (objectAt(next.p) == End) {
      (visited + (next.p -> next), totalSteps + 1)
    } else {
      val neighbors = findValidNeighbors(next.p)
        .foreach{ case (point, dir) => 
          if (dir == next.facing) {
            pq.enqueue(RNode(point, next.score + 1, dir, 1, List(next.p)))
          } else if (isOneStep(dir, next.facing)) {
            pq.enqueue(RNode(point, next.score + 1001, dir, 1 ,  List(next.p)))
          } else {
            pq.enqueue(RNode(point, next.score + 2001, dir, 1, List(next.p)))
          }
        }

      // println("Adding all neighbors:")
      // neighbors.foreach(v => println("\t" + v))
      traverseGrid(pq, visited + (next.p -> next), totalSteps + 1)
    }
  }

  def countNumTiles: Int = {
    val results = startGrid2

    val endingLocResults = results(endLoc)
    val numTiles = results.count((_, finalCost) => finalCost == endingLocResults)
    numTiles
  }

  def startGrid2: Map[Point, Int] = {
    traverseGrid2(reindeerLoc, Right, 0, Set.empty, Map.empty)._1
  }


  case class NodeQueue(score: Int, next: Point, visited: Set[Point], prevDir: Direction) {

    def toVisit(addScore: Int, node: Point, dir: Direction): NodeQueue = {

      NodeQueue(score + addScore, node, visited + node, dir) 
    }
  }
  object NodeQueueOrdering extends Ordering[NodeQueue] {
    def compare(x: NodeQueue, y: NodeQueue): Int = y.score.compare(x.score)
  }


  def startGrid3: Map[Point, Int] = {
    val startingNodeQueue = NodeQueue(0, reindeerLoc, Set.empty, Right)
    val pq = new PriorityQueue[NodeQueue]()(NodeQueueOrdering)
    pq.enqueue(startingNodeQueue)
    traverseGrid3(pq, Map.empty, endLoc)
  }

  def countGrid3Tiles: Int = {
    val grid3Res = startGrid3
    val target = grid3Res(endLoc)

    grid3Res.count(_._2 == target) + 1 
  
  }


  @tailrec
  final def traverseGrid3(pq: PriorityQueue[NodeQueue], res: Map[Point, Int], endLoc: Point): Map[Point, Int] = {
    // TODO CHECK IF endloc has been found and the pq.next is > than that number
    if (pq.isEmpty) {
      res
    } else if (pq.head.score > res.getOrElse(endLoc, Int.MaxValue)) {
      res

    } else if (objectAt(pq.head.next) == End) {
      val next = pq.dequeue
      val updatedResults = next.visited.foldLeft(res) { case (agg, nextVisited) => 
        if (agg.getOrElse(nextVisited, next.score) >= next.score) {
          agg + (nextVisited -> next.score)
        } else {
          agg 
        }
      }

      traverseGrid3(pq, updatedResults, endLoc)
    } else {
      val next = pq.dequeue 
      val neighborsCost = findValidNeighbors(next.next)
        .filter(v => !next.visited.contains(v._1))
        // .filter(v => !res.contains(v._1))
        .foreach{(neighbor, dir) => {
          val additionalCost = if (next.prevDir == dir) {
            1
          } else if (ReindeerOlympics.isOneStep(dir, next.prevDir)) {
            1001
          } else {
            2001
          }

          pq.enqueue(next.toVisit(additionalCost, neighbor, dir))
        }
      }
      traverseGrid3(pq, res, endLoc)
    }
  }

  /**
      * From each spot I need to do a DFS and take the minimum distance between start and
      * end. I need to do this via recursive funciton
      *
      * @param pq
      * @param visited
      * @return
      */
  def traverseGrid2(currPoint: Point, currDir: Direction, stepCost: Int, visited: Set[Point], endingMap: Map[Point, Int]): (Map[Point, Int], Int) = {
    val filteredNeighbors = findValidNeighbors(currPoint)
      .filter(n => !visited.contains(n._1))

    if (objectAt(currPoint) == End) {
      val minSteps = Math.min( endingMap.getOrElse(currPoint, stepCost), stepCost)
      val updatedMap = endingMap + (currPoint -> minSteps)
      (updatedMap, stepCost)
    } else if (filteredNeighbors.isEmpty) {
      val updatedMap =  endingMap + (currPoint -> endingMap.getOrElse(currPoint, Int.MaxValue))

      (updatedMap, Int.MaxValue)
    } else {
      val updatedVisited = visited + currPoint
      filteredNeighbors
      .foldLeft((endingMap, Int.MaxValue)){ case ((aggMap, minCost), (neighbor, dir)) => 
        val neighborCost = if (dir == currDir) {
          stepCost + 1
        } else if (isOneStep(dir, currDir)) {
          stepCost + 1001
        } else {
          stepCost + 2001
        }
       
        val (neighborTravelled, travelledStepCost) = traverseGrid2(neighbor, dir, neighborCost, updatedVisited, aggMap)
        val updatedMapWithNeighbor = neighborTravelled + (currPoint -> Math.min(neighborTravelled.getOrElse(currPoint, Int.MaxValue), travelledStepCost))
        (updatedMapWithNeighbor, Math.min(minCost, travelledStepCost))
      }
    }
  }

  def printGrid: Unit = {
    grid.foreach{ line => 
      line.foreach { _ match
        case ROpen => print('.')
        case RWall => print('#')
        case Start => print('S')
        case End => print('E')

      }
      println("")
    }
  }
}

object ReindeerOlympics {
  def main(args: Array[String]): Unit = {
    val input = Input.readTwentyFourFromResource("day16_input")
    val reindeerOlympics = ReindeerOlympics.parseInput(input)

    val minimumCost = reindeerOlympics.calculateScore
    println("The minimum cost to traverse the grid is: " + minimumCost)

    val gridSix = reindeerOlympics.startTraverseSixDFSThree
    println("Size of grid Six: " + gridSix.size)
  }

  def emptyNodeOrderQueue: PriorityQueue[RNode] = new PriorityQueue()(NodeOrder)
  // This would be eaiser adding values that we can subtract ;) 
  def isOneStep(a: Direction, b: Direction): Boolean = {
    val aInt = dirToInt(a)
    val bInt = dirToInt(b)
    val absDiff = Math.abs(aInt - bInt)

    absDiff == 1 || absDiff == 3
  }
  
  def dirToInt(dir: Direction): Int = {
    dir match {
      case Right => 1
      case Up => 2
      case Direction.Left => 3
      case Down => 4
    }
  }

  def parseInput(in: List[String]): ReindeerOlympics = { 
    val grid = in.zipWithIndex.map { case (line, y) => 
      line.zipWithIndex.map { case (ch, x) =>
        ch match {
          case '#' => RWall
          case 'S' => Start
          case 'E' => End
          case _   => ROpen
        }
      }.toVector
    }.toVector

    val reindeerLoc = for {
      y <- 0 until grid.length
      x <- 0 until grid(0).length
      if grid(y)(x) == Start 
    } yield Point(x, y)

    val endLoc: Point  = (for {
        y <- 0 until grid.length
        x <- 0 until grid(0).length 

        if grid(y)(x) == End
      } yield Point(x, y)).head

    ReindeerOlympics(grid, reindeerLoc.head, Right, endLoc)
  }
}
