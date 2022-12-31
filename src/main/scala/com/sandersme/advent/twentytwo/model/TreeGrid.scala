package com.sandersme.advent.twentytwo.model

case class Tree(height: Int)

type Trees = List[List[Tree]]

case class TreeGrid(trees: Trees) {
  lazy val gridWidth: Int = trees.head.length - 1
  lazy val gridHeight: Int = trees.length - 1

  def treeAt(row: Int, col: Int): Tree = {
    trees(row)(col)
  }

  def isTreeAtEdge(row: Int, col: Int): Boolean = {
    val colAtTop = col == 0
    val rowAtLeft = row == 0
    val colAtBottom = row == gridHeight
    val rowAtRight = col == gridWidth

    colAtTop || colAtBottom || rowAtRight || rowAtLeft
  }

  def isVisibleInCol(row: Int, col: Int, treeHeight: Int): Boolean = {
    //noinspection Duplicates
    val isRowShorterThan: Int => Boolean = r => treeAt(r, col).height < treeHeight

    val isVisibleAbove = (0 until  row).forall(isRowShorterThan)
    val isVisibleBelow = (row + 1 to gridWidth).forall(isRowShorterThan)

    isVisibleAbove || isVisibleBelow
  }

  def isVisibleInRow(row: Int, col: Int, treeHeight: Int): Boolean = {
    //noinspection Duplicates
    val isColShorterThan: Int => Boolean = c => treeAt(row, c).height < treeHeight

    val isLeftVisible = (0 until col).forall(isColShorterThan)
    val isRightVisible = (col + 1 to gridHeight).forall(isColShorterThan)

    isLeftVisible || isRightVisible
  }

  def mergeVisibleUntilBlocked(visible: IndexedSeq[Boolean], blocked: IndexedSeq[Boolean]): List[Boolean] = {
    val f  = blocked.headOption match {
      case None => visible
      case Some(firstBlocking) => visible :+ firstBlocking
    }

    f.toList
  }

  // TODO: POssible abstract away rowvisibility and colvisibility.
  def rowVisibilityScore(row: Int, col: Int): Int = {
    val treeHeight = treeAt(row, col).height
    val isColShorterThan: Int => Boolean = c => treeAt(row, c).height < treeHeight

    val left = (0 until col).map(isColShorterThan).reverse
    val right = (col + 1 to gridHeight).map(isColShorterThan)

    val (leftVisible, leftBlocked) = left.span(identity)
    val (rightVisible, rightBlocked) = right.span(identity)

    val leftTotal = mergeVisibleUntilBlocked(leftVisible, leftBlocked)
    val rightTotal = mergeVisibleUntilBlocked(rightVisible, rightBlocked)

    leftTotal.length * rightTotal.length
  }

  def calculateHighestVisibilityScore: Int = {
    calculateAllVisibilityScores.flatten.max
  }

  def calculateAllVisibilityScores: List[List[Int]] = {
    val visibilityScores = (0 to gridWidth).toList.map{ row =>
      (0 to gridHeight).toList.map{ col =>
        calculateVisibilityScore(row, col)
      }
    }

    visibilityScores
  }

  def calculateVisibilityScore(row: Int, col: Int): Int = {
    colVisibilityScore(row, col) * rowVisibilityScore(row, col)
  }

  def colVisibilityScore(row: Int, col: Int): Int = {
    val treeHeight = treeAt(row, col).height
    val isRowShorterThan: Int => Boolean = r => treeAt(r, col).height < treeHeight

    val above = (0 until  row).map(isRowShorterThan).reverse
    val below = (row + 1 to gridWidth).map(isRowShorterThan)

    val (aboveVisible, aboveBlocked) = above.span(identity)
    val (belowVisible, belowBlocked) = below.span(identity)

    val aboveTotal = mergeVisibleUntilBlocked(aboveVisible, aboveBlocked)
    val belowTotal = mergeVisibleUntilBlocked(belowVisible, belowBlocked)

    aboveTotal.length * belowTotal.length
  }

  def isTreeAtVisible(row: Int, col: Int, treeHeight: Int): Boolean = {
    isTreeAtEdge(row, col) ||
      isVisibleInRow(row, col, treeHeight) ||
      isVisibleInCol(row, col, treeHeight)
  }

  def countTreesVisible: Int = {
    val treeVisibility = (0 to gridWidth).flatMap(row =>
      (0 to gridHeight).map{ col =>
        val treeHeight = treeAt(row, col).height
        isTreeAtVisible(row, col, treeHeight)
      }
    )

    treeVisibility.count(identity)
  }

  def printVisibility: Unit = {
    val treeVisibility = (0 to gridWidth).map(row =>
      (0 to gridHeight).map{ col =>
        val treeHeight = treeAt(row, col).height
        val visibility = isTreeAtVisible(row, col, treeHeight) match {
          case b if b => "X"
          case _ =>      "O"
        }
        print(visibility)
      }
      println()
    )
  }
}

object TreeGrid {
  def parseInput(input: List[String]): TreeGrid = {
    val trees: Trees = input
      .map(line =>
        line.map( treeInput => Tree(treeInput.asDigit)).toList
      )

    TreeGrid(trees)
  }
}
