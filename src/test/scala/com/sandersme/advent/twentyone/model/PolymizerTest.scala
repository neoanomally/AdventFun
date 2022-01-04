package com.sandersme.advent.twentyone.model

class PolymizerTest extends munit.FunSuite {
  val TEST_INPUT = List("NNCB", "", "CH -> B", "HH -> N", "CB -> H", "NH -> C", "HB -> C", "HC -> B", "HN -> C",
  "NN -> C", "BH -> H", "NC -> B", "NB -> B", "BN -> B", "BB -> N", "BC -> B", "CC -> N", "CN -> C")

  val LARGER_TEST_INPUT = "NBCCNBBBCBHCB" :: TEST_INPUT.tail
  val TEST_POLYMIZER = Polymizer.parseInput(TEST_INPUT)

  test("Validate that we can parse test input") {
    //
    val polymizer = Polymizer.parseInput(LARGER_TEST_INPUT)
    val occurrences = polymizer.countOccurrences

    val expectedMap = Map('B' -> 6L, 'C' -> 4L, 'H' -> 1L, 'N' -> 2L)
    assertEquals(occurrences, expectedMap)
  }

  test("Apply one slide of the polymer insertion rules to the template") {
    val appliedOneStep = Polymizer.applyInsertionRulesOnce(TEST_POLYMIZER)
    val expectedOcccurrencesOneStep = Map('N' -> 2L, 'B' -> 2L, 'C' -> 2L, 'H' -> 1L)

    assertEquals(appliedOneStep.countOccurrences, expectedOcccurrencesOneStep)
  }

  test("Apply 10 steps of pair insertions to the polymetemplate.") {
    val tenSteps = Polymizer.applyInsertionRuleNSteps(TEST_POLYMIZER, 10)

    val expected = Map('B' -> 1749L, 'C' -> 298L, 'H' -> 161L)
    val outputMap = tenSteps.countOccurrences
      .filter(value => expected.contains(value._1))

    assertEquals(outputMap, expected)
  }

  test("Validate the max - min after ten steps produces 1588") {
    val tenSteps = Polymizer.applyInsertionRuleNSteps(TEST_POLYMIZER, 10)

    val expectedDifferenceMinMax = 1588L
    val output = tenSteps.diffMaxMin

    assertEquals(output, expectedDifferenceMinMax)
  }

  test("Calculate the difference between min and max for the first matrix") {
    // NN, NC, CB == C = 1, N = 2, B = 1
    val polymizer = Polymizer.parseInput(TEST_INPUT) // Should be 2 - 0
    // TODO Change test on a bigger set maybe step 5
    val minMaxDifference = polymizer.diffMaxMin

    assertEquals(minMaxDifference, 2L)
  }
}
