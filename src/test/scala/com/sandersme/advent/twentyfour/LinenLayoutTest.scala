package com.sandersme.advent.twentyfour

import scala.runtime.stdLibPatches.language.`3.0`

class LinenLayoutTest extends munit.FunSuite {
  test("Validate that we can parse out the patterns") {
    val patterns = TEST_LINEN.patterns

    val expectedPatterns: Boolean = List("r", "wr", "b", "g", "bwu", "rb", "gb", "br").forall(patterns.contains)
    val expectedFinalLinen = "bbrgwb"
    val expectedFirstLinen = "brwrr"

    assert(expectedPatterns)
    assertEquals(TEST_LINEN.linens.head, expectedFirstLinen)
    assertEquals(TEST_LINEN.linens.last, expectedFinalLinen)
  }

  test("validate that the patternsExist method works as expected") {
    assert(TEST_LINEN.patternExists("bbrgwb", 0, 1))
    assert(!TEST_LINEN.patternExists("bbrgwb", 0, 2))

    assert(TEST_LINEN.patternExists("brwrr", 2, 2))
  }

  test("Let's testout our PrefixTree that we created.") {
    val trie = PrefixTrie()

    val doesnot = trie.contains("HellO")
    trie.add("HellO")
    val doesnotTwo = trie.contains("Helm")
    trie.add("Helm")
    val doestwo = trie.contains("Helm")

    val doesnotthree = trie.contains("Hell")
    trie.add("Hell")
    val doesThree =  trie.contains("Hell")
    val does = trie.contains("HellO")
   

    trie.add("HellOdea")

    val subwords = trie.findAllSubwords("HellOd")

    assertEquals(subwords, List("Hell", "HellO"))
    assert(!doesnot)
    assert(does)
    assert(!doesnotTwo)
    assert(!doesnotthree)
    assert(doestwo)
    assert(doesThree)
  }

  test("Check how many different ways we can split a word") {
    val testIn = "brwrr"

    val allSubwords = TEST_LINEN.patterns.findAllSubwords(testIn)
    val expectedAllSubwords = List("b", "br")
    assertEquals(allSubwords, expectedAllSubwords)
  }
  
  test("Start counting num options".ignore) {
    val test = TEST_LINEN.patterns.startCountNumOptions("gbbr")
    val testTwo = TEST_LINEN.patterns.startCountNumOptions("rrbgbr")
    val testThree = TEST_LINEN.patterns.startCountNumOptions("bwurrg")
  
    assertEquals(test, 4L)
    assertEquals(testTwo, 6L)
    assertEquals(testThree, 1L)
  }

  test("find all endIndexes") {
    // 0, 1, 2, 3, 4, 5
    // b, w, u, r, r, g -- I need to grab the MAX INDEX + 1 to update 
     // r, wr, b, g, bwu, rb, gb, br
     // -- idx: 5 ->    g   -- 1
     // -- idx  4 -> r, g but no r g - 1
     // -- idx  3 -> r, r, g -> 1 (1 wy to end)
     // -- idx 2 -> u <- not in so will be empty list
     // -- idx 1 -> w not in so zero 
     // -- idx 0 -> bwu (lookup index 3) - 1
     // -- no rrg there is an r r g 
    val test = TEST_LINEN.patterns.iterativeCountNumOptions("bwurrg") // should get 6
    // println("END INDICIES: TEST: " + test)
  }

  test("validate that all patterns can be found in a test linen") {
    val patternNotExists = TEST_LINEN.isValidPattern("bbrgwb")
    val patternExistsOne = TEST_LINEN.isValidPattern("brwrr")
    val patternExistsTwo = TEST_LINEN.isValidPattern("bggr")      
    val patternExistsThree = TEST_LINEN.isValidPattern("rrbgbr")
    val impossiblePattern = TEST_LINEN.isValidPattern("ubwu")

    assert(!patternNotExists)
    assert(!impossiblePattern)
    assert(patternExistsOne)
    assert(patternExistsTwo)
    assert(patternExistsThree)
  }

  test("Count the number of valid patterns") { 
    val numValid = TEST_LINEN.countNumValidLinens

    assertEquals(numValid, 6)
  }
  
  test("Test me friend".ignore) {
    // TEST_LINEN.linens.map(v => v -> TEST_LINEN.patterns.allSubWordsValid(v))
      // .foreach((l, r) => println(f"$l isValid: $r"))
    // println(TEST_LINEN.linens.map(v => v -> TEST_LINEN.patterns.allSubWordsValid(v))
      // .count(_._2))
  }
  test("Testing thet otal number of distinct pattersn: ")  {
    val shouldEqual16 = TEST_LINEN.countCombinationsOfPatterns
    assertEquals(shouldEqual16, 16L)
  }

  val TEST_INPUT = """r, wr, b, g, bwu, rb, gb, br

      brwrr
      bggr
      gbbr
      rrbgbr
      ubwu
      bwurrg
      brgr
      bbrgwb""".stripMargin
  .split("\n")
  .toList
  .map(_.trim)
  
  val TEST_LINEN = LinenLayout.parseInput(TEST_INPUT)
}
