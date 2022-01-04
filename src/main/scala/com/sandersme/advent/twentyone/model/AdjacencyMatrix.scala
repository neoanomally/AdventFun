package com.sandersme.advent.twentyone.model

case class AdjacencyMatrix(matrix: Vector[Vector[Long]], valueMap: Map[Char, Int]) {

  private lazy val reverseIndex: Map[Int, Char] = valueMap.map(_.swap)

  def printMatrix(): Unit = {
    matrix.foreach(println)
  }

  /**
   * This is a pretty otimized way to go through each value of the matrix... If you see the example below
   * we have a indices for both the row and column is the same character. If we  have pair (N, C) the left
   * part of the pair N is row 1 and the column is C. So to find the number of occurrences of a charcter
   * in this matrix when the matrix represents the "sliding pairs" we need to sum up all the values for
   * the matrix row and column for a given character. For example N is both column 1 and row 1
   *
   * Then we take the sum of the column and row for each character. Since this represent pairs we need to
   * divide the sum by 2. IF the value is sum is odd, we do need to add 1 to the value. This represents
   * the ends that aren't represented by the sliding pairs.
   *
   * By sliding pair I mean NNBC -> NN, NB, BC <- you can see that N and C the front and back are odd.
   * Those two values need added back after you divide by two.
   *
   * Example Matrix:
   *   N  B   C
   * N 0  1   1
   * B 1  1   0
   * C 0  1   0
   * @return
   */
  def countOccurrences: Map[Char, Long] = {
    reverseIndex.map{ case(idx, value) =>
      val sum = reverseIndex.map { case(yIdx, _) =>
        val cellA = matrix(idx)(yIdx)
        val cellB = matrix(yIdx)(idx)

        cellA + cellB
      }.sum

      val modSum = if (sum % 2 != 0) (sum / 2) + 1 else sum / 2

      value -> modSum
    }
  }

  /**
   * We take in two characters, lookup their index for those characters then use those values
   * to do fast eC vector looks ups
   *
   * @return Long value thats located in the matrix cell
   */
  def lookupCell(left: Char, right: Char): Long = {
    val (leftIdx, rightIdx) = (valueMap(left), valueMap(right))

    matrix(leftIdx)(rightIdx)
  }

  /**
   * We take in the left character, right character and the delta update
   * We need to use our valuemap to lookup
   *
   * We also lookup the current value of that cell and add the delta to that update.
   * Then we run the update method on the nested vector cell location. This is eC operations
   * (eC means effectively constant). Then we return a new AdjacencyMatrix with the updated cells
   * @return
   */
  def updateCell(left: Char, right: Char, delta: Long): AdjacencyMatrix = {
    val currentValue = lookupCell(left, right)
    val (leftIdx, rightIdx) = (valueMap(left), valueMap(right))

    val updatedValue = currentValue + delta

    val updatedMatrix = matrix.updated(leftIdx, matrix(leftIdx).updated(rightIdx, updatedValue))

    this.copy(matrix = updatedMatrix)
  }
}

object AdjacencyMatrix {
  /**
   * This is a private method that should only be used in the package. It takes in a
   * set of insertion rules. Goes through every pair and mapping. We use a Set to take
   * only the unique values. Then create an index on those values. This is what creates
   * our valueMap [Char, Int].
   *
   * Once we have our valueMap made we generate a vector of size MxM based on the size
   * of that insertion map.
   *
   * The Matrix and ValueMap are added to a new instance of the Adjacency Matrix.
   * @param insertionRules
   * @return
   */
  private[model] def apply(insertionRules: Map[PolyPair, Char]): AdjacencyMatrix = {
    val lookupValues: Map[Char, Int] = insertionRules.flatMap { case (pair, c) =>
      Set(pair.left, pair.right, c)
    }.toSet
      .zipWithIndex
      .toMap

    val defaultInsertionVectors: Vector[Vector[Long]] = Array
      .ofDim[Long](lookupValues.size, lookupValues.size)
      .map(_.toVector)
      .toVector

    AdjacencyMatrix(defaultInsertionVectors, lookupValues)
  }
}