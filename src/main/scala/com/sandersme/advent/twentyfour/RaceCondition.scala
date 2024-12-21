package com.sandersme.advent.twentyfour

import com.sandersme.advent.Input

// Height is 1 - and same with lenght. Because those are walls that can' tblink through
case class RaceCondition(walls: Walls, start: Point, end: Point, width: Int, height: Int) {
  def printGrid: Unit = {
    (0 until height + 1).map{ y =>
      (0 until width + 1).map{ x =>
        if (x < 1 || y < 1 || x > height || y > height) {
          print("#")
        } else if (walls.contains(Point(x, y))) {
          print("#")
        } else if (x == start.x && y == start.y) {
          print('S')
        } else if (x == end.x && y == end.y) {
          print('E')
        } else {
          print('.')
        }
      }
      println("")
    }
  }

  def neighborPoints(p: Point): List[Point] = {
    neighborPointsIgnoreWall(p)
      .filter(!walls.contains(_))
  }

  def neighborPointsIgnoreWall(p: Point): List[Point] = {
    List(
      Point(p.x + 1, p.y),
      Point(p.x - 1, p.y),
      Point(p.x, p.y + 1),
      Point(p.x, p.y - 1)
    ).filter(np => np.x > 0 && np.y > 0 && np.y < height && np.x < width)
  }

  /// I need to do two things: 1. Starting at the endpoitn Create a Map on the number of picoseconds it takes to
  //get to the end from each spot on the grid. 
  // 2. starting at the end point see where you can "jump to" from the current spot
  
  def endStepMap: Map[Point, Int] = {
    createStepGraph(end)
  }

  def startStepMap: Map[Point, Int] = {
    createStepGraph(start)
  }

  def createStepGraph(findPoint: Point): Map[Point, Int] = {
    def loop(queue: Set[Point], visited: Set[Point], currStep: Int, res: Map[Point, Int]): Map[Point, Int] = {
      // println("Visited: " + visited)
      if (queue.isEmpty) {
        res
      } else {
        val (updatedQueue, map) = queue.foldLeft((Set.empty[Point], res)) { case ((neighborsAgg, map), point) => 
          val updatedNeighbors = neighborsAgg ++ neighborPoints(point).filter(!visited.contains(_))
          val updatedMap = map + (point -> currStep)

          (updatedNeighbors, updatedMap)
        }

        val updatedVisited = visited ++ updatedQueue
        loop(updatedQueue, updatedVisited, currStep + 1, map)
      }
    }

    loop(Set(findPoint), Set(findPoint), 0, Map(end -> 0))
  }

  def findTwoStepsAllPoints(endMap: Map[Point, Int]): List[Int] = {
    (1 until height).foldLeft(List.empty){ case (yAgg, y) =>
      (1 until width).foldLeft(yAgg){ case (xAgg, x) => 
        val curr = Point(x, y)
        walls.contains(curr) match {
          case true => xAgg
          case false => {
            val endDist = endMap(curr)
            val twoSteps = findTwoStepIgnoreWalls(endMap, Point(x, y))
            val shortcutDist = twoSteps
              .toList
              .map(p => endDist - endMap.getOrElse(p, endDist) - 2)
              .filter(_ > 0)
              
            xAgg ++ shortcutDist
          }
        }
      }
    }

  }

  def findTwoStepIgnoreWalls(endMap: Map[Point, Int], point: Point): Set[Point] = {
    def loop(queue: Set[Point], visited: Set[Point], currStep: Int): Set[Point] = {
      if (currStep > 2) {
        queue
      } else {
        val newQueue = queue.foldLeft(Set.empty[Point]){ case(queueAgg, point) => 
          queueAgg ++ neighborPointsIgnoreWall(point)
            .filter(!visited.contains(_))
        }
        
        val updatedVisited = visited ++ newQueue
        loop(newQueue, updatedVisited, currStep + 1)
      }
    }

    loop(Set(point), Set(point), 1)
      .filter(!walls.contains(_))
  }
}
type Walls = Set[Point]

object RaceCondition {

  def main(args: Array[String]): Unit = {
    val input = Input.readTwentyFourFromResource("day20_input")

    val raceCondition = RaceCondition.parseGrid(input)
    val endMap = raceCondition.endStepMap
    val overHundredSaved = raceCondition.findTwoStepsAllPoints(endMap)
      .count(_ >= 100)

    println(f"The number of cheats that save 100 picoseconds: $overHundredSaved")
  }

  def parseGrid(input: List[String]): RaceCondition = {
    val height = input.length - 1
    val width  = input(0).length - 1
    
    val (walls, end, start) = input
      .zipWithIndex
      .foldLeft((Set.empty[Point], Point(0, 0), Point(0, 0))){ case ((yWalls, yE, yS), (line, y)) => 
        line.zipWithIndex.foldLeft((yWalls, yE, yS)) { case ((startWalls, xE, xS), (value, x)) =>
          value match {
            case 'S' => (startWalls, xE, Point(x, y))
            case 'E' => (startWalls, Point(x, y), xS)
            case '#' => (startWalls + Point(x, y), xE, xS)
            case _ => (startWalls, xE, xS)
          }
        }
      }

    RaceCondition(walls, start, end, width, height)

  }



}
