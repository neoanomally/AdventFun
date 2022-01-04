package com.sandersme.advent.twentyone.model

class AdjacencyMatrixTest extends munit.FunSuite {
  val TEST_MATRIX: Vector[Vector[Long]] = Vector(Vector(1, 2, 3, 4, 5, 6), Vector(7, 8, 9, 10, 11, 12),
    Vector(13, 14, 15, 16, 17, 18), Vector(19, 20, 21, 22, 23, 24), Vector(25, 26, 27, 28, 29, 30),
    Vector(31, 32, 33, 34, 35, 36))

  val TEST_INSERTION_RULES: Map[PolyPair, Char] = Map(PolyPair('H', 'H') -> 'C',
    PolyPair('B', 'C') -> 'H', PolyPair('B', 'H') -> 'D')
  val TEST_LOOKUP: Map[Char, Int] = Map('a' -> 0, 'b' -> 1, 'c' -> 2, 'd' -> 3, 'e' -> 4, 'f' -> 5)
  val TEST_ADJACENCY = AdjacencyMatrix(TEST_MATRIX, TEST_LOOKUP)

  test("Test that we can grab a value from a nested vector") {
    val lookupAB = TEST_ADJACENCY.lookupCell('a', 'b')
    val lookupFF = TEST_ADJACENCY.lookupCell('f', 'f')
    assertEquals(lookupAB, 2L)
    assertEquals(lookupFF, 36L)
  }

  test("Test that we can update the value of a vector without changing the original") {
    val originalValue = TEST_ADJACENCY.lookupCell('f', 'f')
    val updatedMatrix = TEST_ADJACENCY.updateCell('f', 'f', 25)
    val afterUpdatingMatrixOriginal = TEST_ADJACENCY.lookupCell('f', 'f')
    val changedValue = updatedMatrix.lookupCell('f', 'f')


    assertEquals(originalValue, afterUpdatingMatrixOriginal)
    assertEquals(changedValue, 25L + 36L)
    assertNotEquals(changedValue, afterUpdatingMatrixOriginal)
  }

  test("Parse from insertion rules") {
    val fromInsertionRules = AdjacencyMatrix.apply(TEST_INSERTION_RULES)

    assertEquals(fromInsertionRules.valueMap.size, 4)
  }

  test("AdjacencyMatrix Insert pairs from starting example") {
    // HHBC
    val fromInsertionRules = AdjacencyMatrix.apply(TEST_INSERTION_RULES)
    val matrix = fromInsertionRules.updateCell('H', 'H', 1L)
      .updateCell('H', 'B', 1L)
      .updateCell('B', 'C', 1L)

    val occurrences = matrix.countOccurrences

    assertEquals(occurrences('H'), 2L)
    assertEquals(occurrences('B'), 1L)
    assertEquals(occurrences('C'), 1L)
    assertEquals(occurrences.values.sum, 4L)
  }
}
