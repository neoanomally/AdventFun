package com.sandersme.advent.twentyfour

import com.sandersme.advent.Input

case class Point(x: Int, y: Int) {
  def incr(xIncr: Int, yIncr: Int): Point = {
    Point(x + xIncr, y + yIncr)
  }
}

// SHould we cache the points in a map?
case class CareGrid(data: List[List[Char]]) {
  def width: Int = data.head.length
  def height: Int = data.length

  def charAt(point: Point): Char = data(point.x)(point.y)

  def validLoc(point: Point): Boolean = {
    point.x >= 0 && point.y >= 0 && point.x < height && point.y < width
  }
 
  def countAllOccurrences(word: String): Int = {
    val occurrences = for {
      x <- 0 until height
      y <- 0 until width
      
    } yield findAllWordsFromPoint(Point(x, y), word, 0, Set(Point(x, y)))

     occurrences.sum 

  }

  def countAllXmas: Int = {
    val xmases = for {
      x <- 1 until height - 1
      y <- 1 until width - 1
      point = Point(x, y)

      if(charAt(point) == 'A')
    } yield containsXMas(point)

    xmases.count(identity)
  }

  def containsXMas(point: Point): Boolean = {
    val diag1 = Set(
      charAt(Point(point.x - 1, point.y - 1)),
      charAt(Point(point.x + 1, point.y + 1))
    )

    val diag2 = Set(
      charAt(Point(point.x + 1, point.y - 1)),
      charAt(Point(point.x - 1, point.y + 1))
    )

    diag1.contains('M') && diag1.contains('S') && diag2.contains('M') && diag2.contains('S')
  }

  def countAllStraightLineOccurrences(word: String): Int = {
    val occurrences = for {
      x <- 0 until height
      y <- 0 until width
    } yield countStraightLines(Point(x, y), word)

    occurrences.sum
  }

  def countStraightLines(point: Point, word: String): Int = {
    List(
      pointsToString(point, word, 0, 1, 0),
      pointsToString(point, word, 0, -1, 0),
      pointsToString(point, word, 0, 0, 1),
      pointsToString(point, word, 0, 0, -1),
      pointsToString(point, word, 0, -1, -1),
      pointsToString(point, word, 0, -1, 1),
      pointsToString(point, word, 0, 1, -1),
      pointsToString(point, word, 0, 1, 1),
    ).sum
  }

  def pointsToString(current: Point, word: String, idx: Int, xIncr: Int, yIncr: Int): Int = {
    if (idx == word.length) {
      1
    } else if (validLoc(current) && charAt(current) == word(idx)) {
      pointsToString(current.incr(xIncr, yIncr), word, idx + 1, xIncr, yIncr)
    } else {
      0
    }
  }

  def findAllWordsFromPoint(point: Point, word: String, idx: Int, visited: Set[Point]): Int = {
    println(f"At point: $point - letter: ${charAt(point)} looking for ${word(idx)} have visited: ${visited}")
    if (idx == word.length - 1 && word(idx) == charAt(point)) {
      println("FOUND")
      1
    } else if (word(idx) == charAt(point)) {
      val neighbors = for {
        xV <- -1 to 1
        yV <- -1 to 1
        p = Point(
          point.x + xV,
          point.y + yV
        )

        if (validLoc(p) && !visited.contains(p))
      } yield p
      
      val nextIdx = idx + 1

      neighbors.map { neighbor =>
        findAllWordsFromPoint(neighbor, word, nextIdx, visited + neighbor)
      }.sum

      // TODO Iterate to all non-visited nodes findAllWordsFromPoint()
    } else {
      0
    }
  }
}

object CareGrid {
  def create(input: List[String]): CareGrid = {
    val chars = input.map(_.toCharArray().toList) 

    CareGrid(chars)
  }

}

object CaresSearch {
  def main(args: Array[String]): Unit = {

    val input = Input.readTwentyFourFromResource("day4_input")
    
    val grid = CareGrid.create(input)

    val sum = grid.countAllStraightLineOccurrences("XMAS")

    val xmasSum = grid.countAllXmas

    println(f"THe number of XMAS occurrences is: $sum")
    println(f"The number of X-MASes is: $xmasSum")
  }

}
