package com.sandersme.advent.twentyone.model

import scala.collection.mutable

type InsertionMap = List[(PolyPair, Char)]

case class Polymizer(adjacencyMatrix: AdjacencyMatrix, insertionMap: InsertionMap) {

  /**
   * This method is just a call through the the adjacencyMatrix for quick lookup
   * @return - Map of the Character and the number of times it's occurred
   */
  def countOccurrences: Map[Char, Long] = adjacencyMatrix.countOccurrences

  /**
   * This just takes our current matrix, runs an update through the AdjacencyMatrix class.
   *
   * @return A new Polymizer with the updated matrix
   */
  def updateMatrixCell(left: Char, right: Char, delta: Long): Polymizer = {
    val updatedMatrix = adjacencyMatrix.updateCell(left, right, delta)
    this.copy(adjacencyMatrix = updatedMatrix)
  }


  /**
   * Fun times, we call the count occurrece, then we iterate through the values with a fold left iteration.
   * We could have just done collection.min && collection.max, but that would go through the occurrence map
   * twice. HOWEVER, we can foldLeft over the values and get min and max in one loop instead of two separate loops
   *
   * ... Now we only call this probably once, I just wanted to do it okay :)
   * @return
   */
  def diffMaxMin: Long = {
    val (min, max) = countOccurrences.foldLeft((Long.MaxValue,
      Long.MinValue)){ case ((min, max), (_, currentCount)) =>
        if (currentCount > max) {
          (min, currentCount)
        } else if (currentCount < min) {
          (currentCount, max)
        } else {
          (min, max)
        }
      }

    max - min
  }
}

object Polymizer {

  /**
   * This does three activities to parse out our initial state:
   * 1.) Create the template. This is the head of the input and we run a sliding window to create
   *     the initial pairs.
   * 2.) Parse out the InsertionRules Map. These rules are Passed to the Adjacency Matrix and
   *     is used for our matrix lookup and generates the empty NxM matrix that is zeroed out
   *     as well as the Map of character to index
   * 3.) We use the starting Template and the default adjacency matrix
   *     and iterate through the matrix applying an update using the original template sliding pairs
   *     setting the default value for those to 1L
   *
   * @param input - Provided by the test case
   * @return - Returns the initialized Polymizer
   */
  def parseInput(input: List[String]): Polymizer = {
    val polymerTemplate = input.head.toCharArray.sliding(2)

    val insertionRules: Map[PolyPair, Char] = input.takeRight(input.length - 2)
      .map(_.split(" -> "))
      .map{ split =>
        val (left, right) = (split.head.head, split.head.last)
        PolyPair(left, right) -> split(1).toCharArray.head
      }.toMap

    val defaultAdjacencyMatrix = AdjacencyMatrix(insertionRules)

    val startingMatrix = polymerTemplate.foldLeft(defaultAdjacencyMatrix){ case(matrix, pair) =>
      matrix.updateCell(pair.head, pair.last, 1L)
    }

    Polymizer(startingMatrix, insertionRules.toList)
  }

  /**
   * This is an iterative appraoch to the polymizer. We have a method that
   * applies one update to a Matrix. Then returning the result after X steps
   *
   * @param polymizer - Takes in the polimizer instance that we are starting with
   * @param steps - The  number of steps to update the polimizer
   * @return - The final Polymizer after stepping through the insertion rules
   */
  def applyInsertionRuleNSteps(polymizer: Polymizer, steps: Int): Polymizer = {
    (1 to steps).foldLeft(polymizer)((accumlatedPolymizer, _) =>
      applyInsertionRulesOnce(accumlatedPolymizer)
    )
  }

  /**
   * This is a bit simpler than it looks. This class has a valueMap
   * that maps our Chars to the index in the matrix, this is for
   * eC fast inserts (yes even with immutable data structures.
   *
   * Each insertion stage we just go through the List of insertion rules
   * and takes the characters from the polyPair and ook up the index
   * for the left and right part of the pair as well as the insertion rule character.
   * Those characters for left and right are to look up the current value at Point(left, right)
   * we have an if statement that is just for the beginning when we may not have a bunch of characters
   * it's just seeing if there is nothing we need to add.
   *
   * Then we emit three different pairs, adding (insert, right), (left, insert) and the third pair
   * is the original pair, but we subtract the value for that to indicate that part of the matrix
   * is split.
   *
   * Once we have that list of changes, we then use a fold left to immutably create an update Map.
   * For each pair we want to tally up the totals. The reason is the insertion rules can create
   * duplicate pairs based on how the insertion rules come into play.
   *
   * Once we have that Map we take our polymizer and iterate through it updating it one
   * pair numAdditions at a time.
   *
   * YES I rewrote this several times ::o ... I probably could have just used
   * a Map of all values, but it was kind of fun going through the matrix route.
   * If I wanted to spend more time I'd figure out how to put the insertion Map
   * into another matrix and just do a bunch of matrix multiplication if that were possible.
   */
  def applyInsertionRulesOnce(polymizer: Polymizer): Polymizer = {
    val pairUpdatePairs = polymizer.insertionMap
      .flatMap{ case(polyPair, insert) =>
        val numInstancesToUpdate = polymizer.adjacencyMatrix.lookupCell(polyPair.left, polyPair.right)

        if(numInstancesToUpdate == 0) {
          List.empty
        } else {
          List(
            PolyPair(insert, polyPair.right) -> numInstancesToUpdate,
            PolyPair(polyPair.left, insert) -> numInstancesToUpdate,
            polyPair -> -numInstancesToUpdate
          )
        }
      }

    val pairUpdateMap = pairUpdatePairs
      .foldLeft(Map.empty[PolyPair, Long]){ case (mapAccumulator, (pair, value)) =>
        val updatedValue = mapAccumulator.getOrElse(pair, 0L) + value
        mapAccumulator + (pair -> updatedValue)
    }

    pairUpdateMap.foldLeft(polymizer){ case (updatedPolymizer, (pair, updateValue)) =>
      updatedPolymizer.updateMatrixCell(pair.left, pair.right, updateValue)
    }
  }
}
