package com.sandersme.advent.twentyfour

import com.sandersme.advent.Input
import com.sandersme.advent.twentyfour.GPoint
import com.sandersme.advent.twentyfour.GPoint
import com.sandersme.advent.twentyfour.GPoint
import com.sandersme.advent.twentyfour.GDir


enum GDir {
  case Left, Right, Up, Down, None
}

case class GPoint(x: Int, y: Int) {
  def generateNeighbors(maxX: Int, maxY: Int): List[(GPoint, GDir)] = {
    List(
      (this.copy(x = x + 1), GDir.Right),
      (this.copy(x = x - 1), GDir.Left),
      (this.copy(y = y + 1), GDir.Down),
      (this.copy(y = y - 1), GDir.Up)
    )
  }

  def isOutOfBounds(maxX: Int, maxY: Int): Boolean = {
    x < 0 || y < 0 || x >= maxX || y >= maxY
  }
}

case class GardenGroups(grid: Vector[Vector[Char]]) {
  def height: Int = grid.length
  def width: Int = grid(0).length
  def valueAt(point: GPoint): Char = { 
    grid(point.y)(point.x)
  }
}

object GardenGroups {
  def main(args: Array[String]): Unit = {
    val startTime = System.currentTimeMillis()
    val input = Input.readTwentyFourFromResource("day12_input")
    val parsedInput = GardenGroups.parseInput(input)
    val gardenAccum = GardenGroups.findAllConnectedPoints(parsedInput)
    val endTime = System.currentTimeMillis()
    println(f"The sum of all area * perimeter is: ${gardenAccum.calculatePlots}")
    println(f"The sum of all area * numEdges is: ${gardenAccum.calculatePlotsPart2}")
    println(f"The total time to calculate: ${endTime - startTime}ms")
  }


  def parseInput(in: List[String]): GardenGroups = {
    val grid = in
      .map(_.toCharArray().toVector)
      .toVector

    GardenGroups(grid)
  }


  case class GardenPlot(area: Int, perimeter: Int, edges: Int) {
    def add(perimeterCounts: Int): GardenPlot = {
      this.copy(area + 1, perimeter + perimeterCounts)
    }

    def addNumEdges(numEdges: Int): GardenPlot = {
      this.copy(edges = numEdges)
    }

    def calculateValuePart2: Int = area * edges
    def calculateValue: Int = area * perimeter
  }

  case class GardenAccum(visited: Set[GPoint], groupPerimeter: Map[GPoint, List[(GPoint, GDir)]], plots: List[GardenPlot]) {
    def calculatePlots: Int = {
      plots.map(_.calculateValue)
        .sum
    }

    def calculatePlotsPart2: Int = {
      plots.map(_.calculateValuePart2)
        .sum
    }

    def updateVisited(point: GPoint): GardenAccum = {
      val updated = visited + point
      this.copy(visited = updated, groupPerimeter + (point -> List.empty))
    }

    def updatePerimeterCount(point: GPoint, dir: GDir): GardenAccum = {
      val updatedCount = groupPerimeter.getOrElse(point, List.empty) :+ (point, dir)
      this.copy(groupPerimeter = groupPerimeter + (point -> updatedCount))
    }

    def calculateNumEdges: Int = {
      val sortedEdges = groupPerimeter.values.flatten.map { case (point, dir) => 
        dir match {
          case GDir.Up  | GDir.Down => (dir, point.y, point.x)
          case GDir.Left | GDir.Right =>  (dir, point.x, point.y)
          case _ => throw new Exception("Error should not get to this point")
        }
      }.groupMap{ case (dir, kv, v) => (dir, kv) }{ case (_, _, v) => v }
        .map{ case (k, v) => k -> v.toList.sorted }
      
      val numContiguousLines = sortedEdges.values
        .map { lines =>
          lines.foldLeft((lines.head, 1)) { case ((prev, result), next) => 
            if (Math.abs(next - prev) > 1) { // There was a "skip" to a new edge
              (next, result + 1)
            } else {
              (next, result)
            }
          }
        }.map(_._2)
        .sum

        numContiguousLines
    }

    def updatePlots: GardenAccum = {
      val gardenPlot = groupPerimeter.foldLeft(GardenPlot(0, 0, 0)){ case (plot, (_, counts)) =>
        plot.add(counts.size)
      }
      
      val numEdges = calculateNumEdges
      // TODO This is ugly break it out and add tests if you ever have time, if not cry.
      this.copy(groupPerimeter = Map.empty, plots = plots :+ gardenPlot.addNumEdges(numEdges))
    }
  }


  object GardenAccum {
    def empty: GardenAccum = GardenAccum(Set.empty, Map.empty, List.empty)
  }

  def findAllConnectedPoints(gardenGrid: GardenGroups): GardenAccum = {
    val points = for {
      y <- 0 until gardenGrid.height
      x <- 0 until gardenGrid.width
    } yield GPoint(x, y)


    points.foldLeft(GardenAccum.empty){ case (accum, point) =>
      if (accum.visited.contains(point)) {
        accum
      } else {
        val value = gardenGrid.valueAt(point)
        val v = visitNeighbors(value, gardenGrid, point, accum, GDir.None)
          v.updatePlots
      }
    }
  }

  def visitNeighbors(value: Char, garden: GardenGroups, point: GPoint, resultAccum: GardenAccum, dir: GDir): GardenAccum = {
    if (garden.valueAt(point) == value && !resultAccum.visited(point)) {
      val accumWithVisited = resultAccum.updateVisited(point)


      point.generateNeighbors(garden.width, garden.height )
        .foldLeft(accumWithVisited) { case (accum, (neighbor, neighborDir)) => 
          val outOfBounds = neighbor.isOutOfBounds(garden.width, garden.height)
          val accumWithCheckPerim = if (outOfBounds || garden.valueAt(neighbor) != value) {
            accum.updatePerimeterCount(point, neighborDir)
          } else {
            accum
          }

          if (accum.visited.contains(neighbor) || outOfBounds) {
            accumWithCheckPerim
          } else {
            visitNeighbors(value, garden, neighbor, accumWithCheckPerim, neighborDir) 
          }
        }
    } else {
      resultAccum
    }
  }
}
