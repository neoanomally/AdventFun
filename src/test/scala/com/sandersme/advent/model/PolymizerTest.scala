package com.sandersme.advent.model

import com.sandersme.advent.model.Polymizer.applyInsertionRulesOnce

class PolymizerTest extends munit.FunSuite {
  val TEST_INPUT = List("NNCB", "", "CH -> B", "HH -> N", "CB -> H", "NH -> C", "HB -> C", "HC -> B", "HN -> C",
  "NN -> C", "BH -> H", "NC -> B", "NB -> B", "BN -> B", "BB -> N", "BC -> B", "CC -> N", "CN -> C")

  val TEST_POLYMIZER = Polymizer.parseInput(TEST_INPUT)

  test("Validate that we can parse test input") {
    val polymizer = Polymizer.parseInput(TEST_INPUT)

    assertEquals(polymizer.template, List[Char]('N', 'N', 'C', 'B'))
    assertEquals(polymizer.insertionMap.size, TEST_INPUT.size - 2)
  }

  test("Apply one slide of the polymer insertion rules to the template") {
    val appliedOneStep = Polymizer.applyInsertionRulesOnce(TEST_POLYMIZER)
    val expectedTemplate = "NCNBCHB".toCharArray.toList
    assertEquals(appliedOneStep.template, expectedTemplate)
  }

  test("Apply 10 steps of pair insertions to the polymetemplate.") {
    val tenSteps = Polymizer.applyInsertionRuleNSteps(TEST_POLYMIZER, 10)

    val expected = Map('B' -> 1749L, 'C' -> 298L, 'H' -> 161L)
    val outputMap = tenSteps.countOccurrences
      .filter(value => expected.contains(value._1))
      .toMap

    assertEquals(outputMap, expected)
  }

  test("Validate the max - min after ten steps produces 1588") {
    val tenSteps = Polymizer.applyInsertionRuleNSteps(TEST_POLYMIZER, 10)

    val expectedDifferenceMinMax = 1588L
    val output = tenSteps.diffMaxMin

    assertEquals(output, expectedDifferenceMinMax)
  }
//
//  test("Apply the polymizer to for 40 steps and get the difference :') :'(") {
//    val fortySteps = Polymizer.applyInsertionRuleNSteps(TEST_POLYMIZER, 40)
//
//    assertEquals(fortySteps.diffMaxMin, 2188189693529L)
//  }
}
