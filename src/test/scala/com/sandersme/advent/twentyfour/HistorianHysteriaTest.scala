import com.sandersme.advent.twentyfour.HistorianHysteria

class HistorianHysteriaTest extends munit.FunSuite {
  val TEST_INPUT = """3   4
      4   3
      2   5
      1   3
      3   9
      3   3""".stripMargin
      .split("\n")
      .toList
      .map(_.trim)
 
  test("Test that I can calculate a similarityScore") {
    val (left, right) = HistorianHysteria.createLists(TEST_INPUT)


    val similiarityScore = HistorianHysteria.calculateSimilarityScore(left, right)

    val expectedSimilarityScore = 31
    assertEquals(similiarityScore, expectedSimilarityScore)
  }
}
