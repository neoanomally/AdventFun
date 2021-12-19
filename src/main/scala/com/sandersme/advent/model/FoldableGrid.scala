package com.sandersme.advent.model

import com.sandersme.advent.graph.Point

import scala.annotation.tailrec


// TODO It'd be more efficient to use Vector instead of List for dot grid gien we are doing index lookups
// Vector also has an update funciton. do it in a bit.
type DotGrid = Vector[Vector[Boolean]]
type FoldablePoint = Point


// TODO FIGURE OUT WHAT FOLDS SHOULD BE MODELLED AS
case class FoldableGrid(grid: DotGrid, folds: List[FoldablePoint]) {
  def printGrid(): Unit = {
    grid.foreach(a => println(a.map{
      case true => "% "
      case false => "  "
    }.mkString("")))
  }

  def countVisibleDots: Int = {
    grid.map(_.count(identity)).sum
  }

}

object FoldableGrid {
  def parseInput(input: List[String]): FoldableGrid = {
    val coordinates: List[Point] = input.takeWhile(_.nonEmpty)
      .map(Point.parseInput)

    val rightCount = input.length - coordinates.size - 1

    val folds: List[FoldablePoint] = input.takeRight(rightCount)
      .map(parseFold)

    val dotGrid = createGridFromCoordinates(coordinates)

    FoldableGrid(dotGrid, folds)
  }

  def foldAllInstructions(foldableGrid: FoldableGrid): FoldableGrid = {
    foldableGrid.folds.indices.foldLeft(foldableGrid)((grid, _) =>
      foldOneStep(grid)
    )
  }

  def foldOneStep(foldableGrid: FoldableGrid): FoldableGrid = {
    if (foldableGrid.folds.isEmpty) {
      foldableGrid
    } else {
      val fold = foldableGrid.folds.head
      val dotGrid = fold.x match {
        case 0 => foldUp(foldableGrid.grid, fold)
        case _ => foldLeft(foldableGrid.grid, fold)
      }

      FoldableGrid(dotGrid, foldableGrid.folds.tail)
    }
  }

  // SPLITTING Into the big part and the small part and the small part is the one
  // We "fold"
  def foldUp(dotGrid: DotGrid, fold: FoldablePoint): DotGrid = {
    val topSize = dotGrid.size - fold.y - 1
    val bottomSize = dotGrid.size - topSize - 1

    val diff = Math.abs(topSize - bottomSize)

    dotGrid.take(fold.y)
      .zipWithIndex.map{ case(rows, y) =>
        rows.zipWithIndex.map{ case (isDot, x) =>
          if(diff > y) {
            isDot
          } else {
            isDot || dotGrid(dotGrid.size - 1 - y + diff)(x)
          }
        }
      }
  }

  def foldLeft(dotGrid: DotGrid, fold: FoldablePoint): DotGrid = {
    val leftSize = dotGrid.head.size - fold.x - 1
    val rightSize = dotGrid.head.size - leftSize - 1
    val diff = Math.abs(rightSize - leftSize)

    dotGrid.zipWithIndex
     .map { case(row, y) =>
       row.take(fold.x).zipWithIndex.map{ case(isDot, x) =>
         val xIdx = dotGrid.head.size - x - 1

         if (diff > x) {
           isDot
         } else {
           isDot || dotGrid(y)(row.size - x - diff - 1)
         }
       }
     }
  }

  private[model] def createGridFromCoordinates(points: List[Point]): DotGrid = {
    val (maxX: Int, maxY: Int) = points.foldLeft((0, 0)){ case ((xMax, yMax), point) =>
      val x = Math.max(point.x, xMax)
      val y = Math.max(point.y, yMax)

      (x, y)
    }

    val pointsSet = points.toSet
    // We are going to use an array FIRST because it's constant time operation
    // for update on array versus O(N) for List since it needs to copy everything
    val dotGrid: Vector[Vector[Boolean]] = Array.ofDim[Boolean](maxY + 1, maxX + 1)
      .zipWithIndex
      .map{ case(row, y) =>
        row.zipWithIndex.map {(_, x) =>
          pointsSet.contains(Point(x, y))
        }.toVector
      }.toVector

    dotGrid
  }

  // fold along y=6
  private def parseFold(str: String): FoldablePoint = {
    val split = str.split("=")
    val axis = split(0).last
    val value = split(1).toInt

    axis match {
      case 'x'    => Point(value, 0)
      case 'y'    => Point(0, value)
      case _      => throw new Exception("Error in foldable value this should be unreachable")
    }
  }
}
