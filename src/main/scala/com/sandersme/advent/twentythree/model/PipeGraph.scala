package com.sandersme.advent.twentythree.model

import com.sandersme.advent.twentyone.graph.Point

import PipeType._
import com.sandersme.advent.twentytwo.model.CommDecoder.loopIncrementCharacters
import com.sandersme.advent.twentythree.PipeMaze.pipeGraph
import scala.annotation.tailrec
import com.sandersme.advent.twentythree.PipeMaze.loopedMap
import com.sandersme.advent.twentythree.model.PipeGraph.toLeft

case class PipeGraph(pipes: Vector[Vector[Pipe]]) {
  val maxX = pipes.head.size
  val maxY = pipes.size


  def printMainLoopWithInnerOuter: Unit = {
    val allInnerPipes = findAllNonFloodedInnerGroups 
      .flatten.toSet

    val allGrounds = findAllGroundGroups
      .flatten.toSet

    
    pipes.foreach{ v => 
      v.foreach{ pipe => 
        if (allInnerPipes.contains(pipe.point)) {
          print("\u001b[1mI")
        } else if (allGrounds.contains(pipe.point)) {
          print("O")
        } else {
          print(pipe)
        }
      }
      println("")
    }
 
  }

  def findStart: Pipe = {
    pipes.flatMap(_.find(_.pipeType == Start))
      .head // this will error out if there is no start, however we know this is the case. 
  }

  def getPipe(point: Point): Pipe = {
    pipes(point.y)(point.x)
  }

  type PointStep = (Point, Int)

  def loopThroughPipes: Map[Point, Int] = {
    // Put the neighbors at the back of the list. 
    def loop(pipe: Pipe, visited: Map[Point,Int], toGo: List[PointStep], s: Int): Map[Point,Int] = {
      val filteredNeighbors = pipe.neighbors
        .filterNot(visited.contains)
        .map(v => (v, s + 1))

      val toGoWIthNeighbors = toGo ++ filteredNeighbors

      if (toGoWIthNeighbors.isEmpty) {
        visited
      } else {
        val (point, step) = toGoWIthNeighbors.head
        val updatedMap = visited + (point -> step)

        loop(getPipe(point), updatedMap, toGoWIthNeighbors.tail, step)
      }
    }

    val startPipe = findStart

    loop(startPipe, Map(startPipe.point -> 0), List.empty, 0)
  }


  /**
    * We need to find the main loop then convert ANY pipe not in the main loop into a
    * ground item. This let's us figure out how to find all the inner tiles.   
    */

   def transformNonMainPipesToGround: PipeGraph = {
     val pipesInMainLoop = loopThroughPipes

     val updatedPipesNotInMain = pipes.map(_.map{ pipe =>  
        if (pipesInMainLoop.contains(pipe.point)) {
          pipe
        } else {
          val updatedNeighbors = PipeGraph.findNeighbors(Ground, pipe.point, maxX, maxY)
          pipe.copy(pipeType = Ground, neighbors = updatedNeighbors)
        }
     })

     val updatedGroundPipeNeighbors = updatedPipesNotInMain.map(_.map{ pipe =>
       if (pipe.pipeType == Ground) {
         val updatedNeighbors = pipe.neighbors.filter{ neighbor =>
            val neighborPipe = updatedPipesNotInMain(neighbor.y)(neighbor.x)
            PipeGraph.canTravelTo(pipe, neighborPipe)
          }
          pipe.copy(neighbors = updatedNeighbors)
        } else {
          pipe
        }
     })

     PipeGraph(updatedGroundPipeNeighbors)
   }    

  /**
    * This method is broken up into two parts. At the most granular once we find a ground
    * node that we haven't visited we find all ground nodes in that region. That makes up
    * a new group of vectors.
    *
    * The second part goes through every ground element. We need to find a ground tile
    * that hasn't been found yet and start there then find all it's neighbors in a given
    * region. 
    *
    * This should find us groups of regions of grounds. 
    *
    * @return
    */
  def findAllGroundGroups: Vector[Set[Point]] = {
    val allGroundTiles = pipes.flatMap(_.filter(_.pipeType == Ground))

    @tailrec
    def loop(toVisit: Vector[Pipe], visited: Set[Point], results: Vector[Set[Point]]): Vector[Set[Point]] = {
      val next = toVisit.headOption

      next match {
        case None => results
        case Some(pipe) if visited.contains(pipe.point) =>
          loop(toVisit.tail, visited, results)
        case Some(pipe) =>
          val allNeighborsInRegion = visitAllNeighbors(pipe.neighbors, Set(pipe.point))
          val updatedResults = results :+ allNeighborsInRegion
          val updatedVisited = visited ++ allNeighborsInRegion

          loop(toVisit.tail, updatedVisited, updatedResults)
      }
    }

    @tailrec
    def visitAllNeighbors(neighborToVisit: List[Point], visited: Set[Point]): Set[Point] = {
      val nextPipe = neighborToVisit.headOption.map(getPipe)

      nextPipe match {
        case None => visited
        case Some(pipe) if visited.contains(pipe.point) => 
          visitAllNeighbors(neighborToVisit.tail, visited) 
        case Some(pipe) =>
          val updatedNeighborsToVisit = neighborToVisit.tail ++ pipe.neighbors
          val updatedVisited = visited + pipe.point
          visitAllNeighbors(updatedNeighborsToVisit, updatedVisited) 
      }   
    }

    loop(allGroundTiles, Set.empty, Vector.empty)
  }

  /**
    * For each inner loop found we need to follow the pipes to see if they squeezee
    * outside. If they do; remove the innerground from the  
    */
  def findAllNonFloodedInnerGroups: Vector[Set[Point]] = {
    val mainLoop = loopThroughPipes
    val allInnerGroups = findAllInnerGroups
    val distinctPotentialInnerTiles = allInnerGroups.flatten.toSet

    allInnerGroups
      .filter(group => followPipesNotFlood(group, distinctPotentialInnerTiles))
  }

  def followPipesNotFlood(innerGroup: Set[Point], allInnerTiles: Set[Point]): Boolean = {
    val topMostPoint = innerGroup.minBy(point => (point.y, point.x))

    def isOutOfBounds(point: Point): Boolean = {
      point.x < 0 || point.y < 0 || point.x >= maxX || point.y >= maxY
    }

    def walkOneDirection(currPoint: Point, stepFn: (Point) => Point, pipeIgnore: PipeType, count: Int = 0): Boolean = {
      lazy val pipe = getPipe(currPoint) // Not worried about going out of bounds should hit ground before then
      lazy val neighborsEmpty = pipe.neighbors.isEmpty
      lazy val nextPoint = stepFn(currPoint)
      val ignoreEach = Set(pipeIgnore, Ground)
      // GROUND type and is not an InnerGround
      if (isOutOfBounds(currPoint) || pipe.pipeType == Ground && !allInnerTiles.contains(currPoint)) {
        count % 2 == 1
      } else if (ignoreEach.contains(pipe.pipeType)) {
        walkOneDirection(nextPoint, stepFn,pipeIgnore, count)
      } else {
        walkOneDirection(nextPoint, stepFn, pipeIgnore, count + 1)
      }
    }
 
    // GO each direction and make sure we have an odd number of non-ground pipes
    List((PipeGraph.goLeft, Horizontal), (PipeGraph.goRight, Horizontal), 
      (PipeGraph.goUp, Vertical), (PipeGraph.goDown, Vertical))
        .map((fn, pipeIgnore) => walkOneDirection(topMostPoint, fn, pipeIgnore))
        .reduce(_ || _)
  }

  def findAllInnerGroups: Vector[Set[Point]] = {
    findAllGroundGroups
      .filter(group => PipeGraph.isInnerGroundGroup(group, maxX, maxY))
  }

  def countTilesInnerGroups: Int = {
    findAllNonFloodedInnerGroups
      .flatten
      .size
  }

  val topConnectedVerticalPipes = Array(Vertical, NEBend, NWBend)
  def countTilesInsideLoops: Int = {
    val allInnerGroup = loopThroughPipes

    val nonInnerGroup = for {
      x <- 0 until maxX
      y <- 0 until maxY

      if (!allInnerGroup.contains(Point(x, y)))
    } yield Point(x, y)

    nonInnerGroup.count(isInBounds)
  }

  def isInBounds(point: Point): Boolean = {
    def countingLoop(currPoint: Point, count: Int): Int = {
      if (currPoint.x < 0) {
        count
      } else {
        val updatedCount = count + topConnectedVerticalPipes.count(_ == getPipe(currPoint).pipeType)
        countingLoop(currPoint.toLeft, updatedCount)
      }
    }

    countingLoop(point, 0) % 2 == 1
  }
}

case class GroundState()
object GroundState { def empty: GroundState = GroundState() }


case class Pipe(pipeType: PipeType, point: Point, neighbors: List[Point]) {
  override def toString(): String = pipeType match {
    case Horizontal => "-"
    case Vertical   => "|"
    case NEBend     => "L"
    case NWBend     => "J"
    case SWBend     => "7"
    case SEBend     => "F"
    case Ground     => "."
    case Start      => "S"
  }
}

enum PipeType {
  case Horizontal // -
  case Vertical   // | 
  case NEBend     // L 
  case NWBend     // J 
  case SWBend     // 7 
  case SEBend     // F
  case Ground     // .
  case Start      // Start Starting Pos
}

object PipeGraph {

  def goLeft(point: Point): Point = point.copy(x = point.x - 1)
  def goRight(point: Point): Point = point.copy(x = point.x + 1)
  def goUp(point: Point): Point = point.copy(y = point.y - 1)
  def goDown(point: Point): Point = point.copy(y = point.y + 1)

  def getPipe(pipes: Vector[Vector[Pipe]], point: Point): Pipe = {
    pipes(point.y)(point.x)
  }


  def parseInput(input: List[String]): PipeGraph = {
    val maxY = input.length
    val maxX = input.head.size

    val pipes = input.zipWithIndex.map { case (line, y) =>
      line.zipWithIndex.map { case (character, x) =>
        val point = Point(x, y)
        val pipeType = parseTile(character)
        val neighbors = findNeighbors(pipeType, point, maxX, maxY)

        Pipe(pipeType, point, neighbors)
      }.toVector
    }.toVector

    val pipesWithTraversableNeighbors = pipes.map(_.map { case pipe =>
      val updatedNeighbors = pipe.neighbors.filter{ neighbor =>
        val neighborPipe = pipes(neighbor.y)(neighbor.x)
        canTravelTo(pipe, neighborPipe)
      }

      pipe.copy(neighbors = updatedNeighbors)
    })

    PipeGraph(pipesWithTraversableNeighbors)
  }

  extension(point: Point) {
    def toRight = point.copy(x = point.x + 1)
    def toLeft  = point.copy(x = point.x - 1)
    def toDown  = point.copy(y = point.y + 1)
    def toUp    = point.copy(y = point.y - 1)
  }

  def findNeighbors(pipeType: PipeType, currPoint: Point, maxX: Int, maxY: Int): List[Point] = {
    val points = pipeType match {
      case Horizontal => List(currPoint.toLeft, currPoint.toRight) 
      case Vertical   => List(currPoint.toUp, currPoint.toDown)
      case NWBend     => List(currPoint.toUp, currPoint.toLeft)
      case NEBend     => List(currPoint.toUp, currPoint.toRight)
      case SWBend     => List(currPoint.toLeft, currPoint.toDown)
      case SEBend     => List(currPoint.toDown, currPoint.toRight) 
      case Ground     => List(currPoint.toRight, currPoint.toLeft, currPoint.toUp, currPoint.toDown) 
      case Start      => List(currPoint.toRight, currPoint.toLeft, currPoint.toUp, currPoint.toDown) 
    }

    points
      .filter(p => p.x < maxX && p.y < maxY)
      .filter(p => p.x >= 0 && p.y >= 0)
  }

  def canTravelTo(origin: Pipe, destination: Pipe): Boolean = {
    if(origin.pipeType == Start && destination.pipeType == PipeType.Ground) {
      false
    } else if (origin.pipeType == Ground) {
      destination.pipeType == Ground
    } else {
      destination.neighbors.contains(origin.point)
    }
  }

  def isInnerGroundGroup(groundGroup: Set[Point], maxX: Int, maxY: Int): Boolean = {
    groundGroup.forall(point => 
      point.x > 0 && point.y > 0 &&
        point.x < maxX - 1 && point.y < maxY - 1
    ) 
  }

  def parseTile(input: Char): PipeType = {
    input match {
      case '-' => Horizontal
      case '|' => Vertical
      case 'L' => NEBend
      case 'J' => NWBend 
      case '7' => SWBend 
      case 'F' => SEBend
      case '.' => Ground 
      case 'S' => Start 
      case t@_   => throw new Exception(s"ERROR ${t} is not a valid tile")
    }
  }
}
