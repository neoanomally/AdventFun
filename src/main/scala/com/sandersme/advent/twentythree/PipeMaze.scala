package com.sandersme.advent.twentythree

import com.sandersme.advent.Input
import com.sandersme.advent.twentythree.model.PipeGraph
import com.sandersme.advent.twentythree.MPipe.*
import com.sandersme.advent.twentythree.Dir.*
import scala.annotation.tailrec

object PipeMaze {

  def main(args: Array[String]): Unit = {

    val input = Input.readTwentyThreeFromResource("day10_input")
    val startTime = System.currentTimeMillis()
    val pipeMaze = PipeMazeV2.parseInput(input)

    val furthestPipeStep = pipeMaze.bfsFurthestStep
    

    val removedPipes = pipeMaze.removePipesNotInLoop
    val floodedMaze = removedPipes.floodOuterBoard
    val floodedOpenings = removedPipes.floodOpenings(floodedMaze)

    val numInnerPIpes = removedPipes.mazeSize -  floodedOpenings.size
    val totalTime = System.currentTimeMillis - startTime

    removedPipes.printMaze(floodedOpenings)

    println("The furthest pipe from the start is: " + furthestPipeStep + " took : " + totalTime + "ms")
    println("The number of inner pipes flooded: " + numInnerPIpes)
  }
  

}

enum Dir {
  case N, E, S, W
}

enum MPipe { 
  case V, H, NE, NW, SE, SW, G, START

}

case class PipeMaze(maze: Vector[Vector[MPipe]], start: (Int, Int)) {
  val height: Int = maze.length
  val width: Int = maze(0).length

  val mazeSize: Int = height * width

  def findTwoNeighbors(x: Int, y: Int): List[(Int, Int, Dir)] = {
    val pipe = maze(y)(x)
  
    findAllNeighbors(x, y)
      .filter{ case (nx, ny, dir) => 
        val neighborPipe = maze(ny)(nx)
        PipeMazeV2.canConnect(pipe, neighborPipe, dir)
      }
  }

  def findAllNeighbors(x: Int, y: Int): List[(Int, Int, Dir)] = {
    List(
      (x + 1, y, E),
      (x - 1, y, W),
      (x, y + 1, S),
      (x, y - 1, N)
      ).filter((nx, ny, _) => nx >= 0 && ny >= 0 && nx < width && ny < height)
  }

   def floodOpenings(visited: Set[(Int, Int)]): Set[(Int, Int)] = {
    visited
      .filter((x, y) => x > 0 && y > 0 && x < width - 1 && y < height - 1) // Remove boarders
      .foldLeft(visited){ case (visitedAgg, (x, y)) => 
        val neighborOpenings = findAllNeighbors(x, y)
          .filter((nx, ny, _) => !visited.contains((nx, ny)))
          .filter{ (nx, ny, dir) => 
            val neighborPipe = maze(ny)(nx)
            dir match {
              case E => neighborPipe == NE || neighborPipe == SE
              case W => neighborPipe == NW || neighborPipe == SW 
              case N => neighborPipe == NW || neighborPipe == NE 
              case S => neighborPipe == SW || neighborPipe == SE
            }
          }

          neighborOpenings.foldLeft(visitedAgg){ case (openingsAgg, (x, y, dir)) =>
            followPipe(x, y, dir, maze(y)(x), openingsAgg, true) 
          }
        }
    }

  @tailrec
  final def followPipe(x: Int, y: Int, dir: Dir, prevPipe: MPipe, visited: Set[(Int, Int)], firstStep: Boolean): Set[(Int, Int)] = {
    val pipe = maze(y)(x)
    val updatedPrevPipe = if (pipe != G && pipe != H && pipe != V) pipe else prevPipe
    val filteredNeighbor = findTwoNeighbors(x, y)
      .filter((nx, ny, _) => !visited.contains((nx, ny))) 
   
    val nextNeighbor = if (firstStep) filteredNeighbor.filter((_, _, nDir) => nDir == dir) else filteredNeighbor

    // println(f"Moving in ${dir} currently at ${pipe} neighbors: ${nextNeighbor}" )
    assert(nextNeighbor.size < 2)

    val updateVisited = dir match {
      case E if pipe == SE || (pipe == H && prevPipe == SE) => floodDirection(x, y, visited, N)// checkNorth
      case E if pipe == NE || (pipe == H && prevPipe == NE) => floodDirection(x, y, visited, S)// / checkSouth
      case W if pipe == SW || (pipe == H && prevPipe == SW) => floodDirection(x, y, visited, S)// // CHeck South
      case W if pipe == NW || (pipe == H && prevPipe == NW) => floodDirection(x, y, visited, N)// // Check North
      case N if pipe == NW || (pipe == V && prevPipe == NW) => floodDirection(x, y, visited, E)// // Check East
      case N if pipe == NE || (pipe == V && prevPipe == NE) => floodDirection(x, y,visited,  W)// // Check West
      case S if pipe == SW || (pipe == V && prevPipe == SW) => floodDirection(x, y, visited, E)// // CHECK East
      case S if pipe == SE || (pipe == V && prevPipe == SE) => floodDirection(x, y, visited, W)// // check west
      case _ => visited 
    }

    val updatedDir = dir match {
      case _ if firstStep => dir
      case E | W if pipe == H => dir
      case N | S if pipe == V => dir
      case E if pipe == NW => N
      case E if pipe == SW => S
      case W if pipe == SE => S
      case W if pipe == NE => N
      case N if pipe == SW => W 
      case N if pipe == SE => E 
      case S if pipe == NW => W 
      case S if pipe == NE => E
      case _ => throw new Exception(f"ERROR Can't go this direction $dir $pipe")
    }

    if (nextNeighbor.isEmpty)
      updateVisited + ((x, y))
    else 
      val (nx, ny, ndir) = nextNeighbor.head
      followPipe(nx, ny, updatedDir, updatedPrevPipe, updateVisited + ((x, y)), false)
  }

  def floodDirection(x: Int, y: Int, visited: Set[(Int, Int)], dir: Dir): Set[(Int, Int)] = {
    val (nx, ny) = dir match {
      case N => (x, y - 1)
      case S => (x, y + 1)
      case E => (x - 1, y)
      case W => (x + 1, y)
    }


    if (nx < 0 || ny < 0 || nx >= width || ny >= height || maze(ny)(nx) !=G) {
      visited
    } else {
      flood(Set((nx, ny)), visited)
    }
  }

  def flood(queue: Set[(Int, Int)], visited: Set[(Int, Int)]): Set[(Int, Int)] = {
    if (queue.isEmpty) {
      visited
     } else {
       val nextQueue = queue.flatMap((x, y) => findAllNeighbors(x, y))
          .map((x, y, _) => (x, y))
          .filter(point => !visited.contains(point))
          .filter((x, y) => maze(y)(x) == G)

        flood(nextQueue, visited ++ queue)
     }
  }

  def floodOuterBoard: Set[(Int, Int)] = {
    // I should find all ground units on the edge of the board as starting points
    val edgePoints = (0 until height).flatMap { y =>
      List((0, y), (width - 1, y))
    } ++ (0 until width).flatMap { x =>
      List((x, 0), (x, height - 1))
    }

    edgePoints.foldLeft(Set.empty[(Int, Int)]) { case (visited, (x, y)) =>
      if (maze(y)(x) == G && !visited.contains((x, y))) {
        flood(Set((x, y)), visited)
      } else {
        visited
      }
    }
  }

  /**
    * to flood board there we need to find all "inlets"
    * an inlet is defined as follows: 
    * when looking at northern neighbors -> J left of L // follow those pipes north 
    * when looking at eastern neighbors  -> L above F // follow those pipes east
    * when looking at southern neighbors -> 7 left of F // follow those pipes south
    * when looking at western neighbors -> J above 7 // follow those pipes west
    */

  def bfsFurthestStep: Int = {
    def loop(queue: Set[(Int, Int)], visited: Set[(Int, Int)], step: Int): Int = {
      if (queue.isEmpty) {
        step 
      } else {
        val nextQueue = queue
          .flatMap((x, y) => findTwoNeighbors(x, y))
          .map((x, y, _) => (x, y))
          .filter(pair => !visited.contains(pair))
        loop(nextQueue, visited ++ queue, step + 1)
      }
    }

    loop(Set(start), Set.empty, -1)
  }

  def bfsAllInLoop: Set[(Int, Int)] = {
    def loop(queue: Set[(Int, Int)], visited: Set[(Int, Int)]): Set[(Int, Int)] = {
      if (queue.isEmpty) {
        visited
      } else {
        val nextQueue = queue
          .flatMap((x, y) => findTwoNeighbors(x, y))
          .map((x, y, _) => (x, y))
          .filter(pair => !visited.contains(pair))
          loop(nextQueue, visited ++ queue)
      }
    }

    loop(Set(start), Set.empty)
  }

  def removePipesNotInLoop: PipeMaze = {
    val allInLoop = bfsAllInLoop

    val updatedMaze = maze.zipWithIndex.map { case (line, y) =>
      line.zipWithIndex.map { case (pipe, x) =>
        if (!allInLoop.contains((x, y))) {
          G 
        } else {
          pipe
        }
      }
    }

    this.copy(maze = updatedMaze)
  }


  def printMaze: Unit = {
    maze.foreach { case line =>
      line.foreach { _  match 
        case G => print('.')
        case H => print('-')
        case V => print('|')
        case NE => print('L')
        case NW => print('J')
        case SE => print('F')
        case SW => print('7')
        case START => print('S')
      }
      println("")
    }
  }


  def printMaze(visited:Set[(Int, Int)]): Unit = {
    val mainLoop = this.bfsAllInLoop
    var F: Int = 0

    maze.zipWithIndex.foreach { case (line, y) =>
      line.zipWithIndex.foreach { case (pipe, x) => 
        if (mainLoop.contains((x, y))) {
          pipe match {
            case G => print('.')
            case H => print('-')
            case V => print('|')
            case NE => print('L')
            case NW => print('J')
            case SE => print('F')
            case SW => print('7')
            case START => print('S')
          }
        } else if (visited.contains((x, y))) {
          print('O')
        } else {
          F += 1
          print('I')
        }
      }
      println("")
    }

    println("Number of F: " + F)
  }
}

object PipeMazeV2 {
/**
  * | is a vertical pipe connecting north and south.
- is a horizontal pipe connecting east and west.
L is a 90-degree bend connecting north and east.
J is a 90-degree bend connecting north and west.
7 is a 90-degree bend connecting south and west.
F is a 90-degree bend connecting south and east.
. is ground; there is no pipe in this tile.
S is the starting position of the animal; there is a pipe on this tile, but your sketch doesn't show what shape the pipe has.
  *
  * @param in
  * @return
  */
  def parseInput(in: List[String]): PipeMaze = {
    val graph = in.map { line =>
      line.map { _ match 
        case '|' => V  
        case '-' => H 
        case 'S' => START
        case 'L' => NE 
        case 'J' => NW 
        case '7' => SW 
        case 'F' => SE 
        case '.' => G 
        case _   => throw new Exception("Error should not have any value of this type")
      }.toVector
    }.toVector

    val height = graph.length
    val width = graph(0).length

    def findStart(x: Int, y: Int): (Int, Int) = {
      val updatedX = if (x >= width) 0 else x
      val updatedY = if (x >= width) y + 1 else y

      if (updatedY >= height) {
        throw new Exception("ERROR SHould be able to find start point")
      } else if(graph(updatedY)(updatedX) == START) {
          (updatedX, updatedY)
      } else {
        findStart(updatedX + 1, updatedY)
      }
    }
    
    val start = findStart(0, 0)

    PipeMaze(graph, start)
  }
  
  def canConnect(left: MPipe, right: MPipe, dir: Dir): Boolean = {
    left match {
      case START | V | NE | NW if dir == N => right == V || right == SE || right == SW
      case START | V | SE | SW if dir == S => right == V || right == NE || right == NW
      case START | H | NE | SE if dir == E => right == H || right == SW || right == NW 
      case START | H | NW | SW if dir == W => right == H || right == SE || right == NE
      case _ => false
    }
  }
}

