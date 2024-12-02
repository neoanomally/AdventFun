package com.sandersme.advent.twentythree.model

import Diagnostic._

class HotSpringDiagnosticsTest extends munit.FunSuite { 
  val TEST_INPUT = """???.### 1,1,3
              |.??..??...?##. 1,1,3
              |?#?#?#?#?#?#?#? 1,3,1,6
              |????.#...#... 4,1,1
              |????.######..#####. 1,6,5
              |?###???????? 3,2,1""".stripMargin.split("\n").toList

  test("Parse test input into a List of HotSpringsDiagnostics") {
    val hostspringDiagnostics = HotSpringsDiagnostics.parseInput(TEST_INPUT) 

    val expectedHeadDiagnostics = List(
      Unknown, Unknown, Unknown, Working, Broken, Broken, Broken 
    )

    val expectedHeadGroups = List(1, 1, 3)
    val expectedHotSpringDiagnostic = 
      HotSpringsDiagnostics(expectedHeadDiagnostics, expectedHeadGroups)

    assertEquals(hostspringDiagnostics.head, expectedHotSpringDiagnostic)
  }

  test("validate that we can find all possible diagnostic routes") {
    val singleLineInput = "?###???????? 3,2,1"

    val parsedInput = HotSpringsDiagnostics.parseSingleInput(singleLineInput)
    val forkingBranches = HotSpringsDiagnostics
      .branchingDiagnostics(parsedInput.diagnostics)

    val expectedNumberOfForkingBranches = 512
    val numberForkingBranches = forkingBranches.size

    assertEquals(numberForkingBranches, expectedNumberOfForkingBranches)
  }

  test("Count the number of contiguous broken diagnostics") {
    val testInput = "#.#.### 1,1,3"
    val singleInputParsed = HotSpringsDiagnostics.parseSingleInput(testInput)

    val diagnosticCounts = HotSpringsDiagnostics.countContiguousBroken(singleInputParsed.diagnostics)
    val expectedCounts: List[Int] = singleInputParsed.damagedGroups

    assertEquals(diagnosticCounts, expectedCounts)
  }
  
  test("Count all combinations of contiguous broken branches.") {
    val singleLineInput = "?###???????? 3,2,1"

    val parsedInput = HotSpringsDiagnostics.parseSingleInput(singleLineInput)

    val numberForkingBranches = parsedInput.countValidBranches
    val expectedNumberOfForkingBranches = 10

    assertEquals(numberForkingBranches, expectedNumberOfForkingBranches)
  }

  test("Count all possible arrangements acrossa ll the diagnostics") {
    val parsedInput = HotSpringsDiagnostics.parseInput(TEST_INPUT)
    
    val numAllPossibleArrangements = HotSpringsDiagnostics.countAllPossibleArrangements( parsedInput)

    val expectedNumArrangements = 21

    assertEquals(numAllPossibleArrangements, expectedNumArrangements)
  }

  test("Unfold test input") {
    val testHotDiagnostics = HotSpringsDiagnostics(
        List(Working, Broken),
        List(1)
      )

    val unfolded = testHotDiagnostics.unfold
    val expectedUnfolded = HotSpringsDiagnostics(
        List(Working, Broken, Unknown, Working, Broken, Unknown,
             Working, Broken, Unknown, Working, Broken, Unknown,
             Working, Broken),
         List(1, 1, 1, 1, 1)
      )

    assertEquals(unfolded, expectedUnfolded)
  }


  test("Test something new") {
    println("hello world")
  }
}
