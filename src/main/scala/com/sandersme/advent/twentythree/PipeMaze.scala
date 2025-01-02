package com.sandersme.advent.twentythree

import com.sandersme.advent.Input
import com.sandersme.advent.twentythree.model.PipeGraph
import com.sandersme.advent.twentythree.MPipe.*
import com.sandersme.advent.twentythree.Dir.*

object PipeMaze {

  def main(args: Array[String]): Unit = {

    val input = Input.readTwentyThreeFromResource("day10_input")
    val startTime = System.currentTimeMillis()
    val pipeMaze = PipeMazeV2.parseInput(input)

    val furthestPipeStep = pipeMaze.bfsFurthestStep
    val totalTime = System.currentTimeMillis - startTime
    println("The furthest pipe from the start is: " + furthestPipeStep + " took : " + totalTime + "ms")
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

  def findTwoNeighbors(x: Int, y: Int): List[(Int, Int)] = {
    val pipe = maze(y)(x)

    List(
      (x + 1, y, E),
      (x - 1, y, W),
      (x, y + 1, S),
      (x, y - 1, N)
    ).filter((nx, ny, _) => nx >= 0 && ny >= 0 && nx < width && ny < height)
    .filter{ case (nx, ny, dir) => 
      val neighborPipe = maze(ny)(nx)
      PipeMazeV2.canConnect(pipe, neighborPipe, dir)
    }.map((nx, ny, _) => (nx, ny))
  }

  def floodBoard: Set[(Int, Int)] = {
    def loop(queue: Set[(Int, Int)], visited: Set[(Int, Int)]): Set[(Int, Int)] = {
      if (queue.isEmpty) {
        visited
       } else {
        val nextQueue = queue.flatMap((x, y) => findTwoNeighbors(x, y))
          .filter(point => !visited.contains(point))
          .filter((x, y) => maze(y)(x) == G)

        loop(nextQueue, visited ++ queue)
       }
    }

    // I should find all ground units on the edge of the board as starting points
    val edgePoints = (0 until height).flatMap { y =>
      List((0, y), (width - 1, y))
    } ++ (0 until width).flatMap { x =>
      List((x, 0), (x, height - 1))
    }

    edgePoints.foldLeft(Set.empty[(Int, Int)]) { case (visited, (x, y)) =>
      if (maze(y)(x) == G && !visited.contains((x, y))) {
        loop(Set((x, y)), visited)
      } else {
        visited
      }
    }
  }

  def bfsFurthestStep: Int = {
    def loop(queue: Set[(Int, Int)], visited: Set[(Int, Int)], step: Int): Int = {
      if (queue.isEmpty) {
        step 
      } else {
        val nextQueue = queue
          .flatMap((x, y) => findTwoNeighbors(x, y))
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
          .filter(pair => !visited.contains(pair))
          loop(nextQueue, visited ++ queue)
      }
    }

    loop(Set(start), Set.empty)
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

