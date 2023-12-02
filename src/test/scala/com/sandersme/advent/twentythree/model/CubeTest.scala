package com.sandersme.advent.twentythree.model

class CubeTest extends munit.FunSuite {
  val TEST_INPUT: List[String] =
    """Game 1: 3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green
      |Game 2: 1 blue, 2 green; 3 green, 4 blue, 1 red; 1 green, 1 blue
      |Game 3: 8 green, 6 blue, 20 red; 5 blue, 4 red, 13 green; 5 green, 1 red
      |Game 4: 1 green, 3 red, 6 blue; 3 green, 6 red; 3 green, 15 blue, 14 red
      |Game 5: 6 red, 1 blue, 3 green; 2 blue, 1 red, 2 green
      |""".stripMargin
      .split("\n")
      .toList

  val TEST_CUBE_MAP: BagCubeContents = BagCubeContents(Map(
    CubeColor.red -> 12,
    CubeColor.green -> 13,
    CubeColor.blue -> 14
  ))


  test("Test to make sure first line parses correctly") {
    val input = "Game 1: 3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green"

    val gameSets = List(
      GameSet(List(Cube(3, CubeColor.blue), Cube(4, CubeColor.red))),
      GameSet(List( Cube(1, CubeColor.red), Cube(2, CubeColor.green), Cube(6, CubeColor.blue))),
      GameSet(List(Cube(2, CubeColor.green)))
    )

    val expectedParsed = CubeGame(1,  gameSets)

    val parsed: CubeGame = Cube.parseInput(input)

    assertEquals(parsed, expectedParsed)
  }

  test("Test that we can parse the cube set: 3 blue, 4 red") {
    val input = "3 blue, 4 red"

    val parsed: GameSet = Cube.parseCubeSet(input)
    val expected = GameSet(List(Cube(3, CubeColor.blue), Cube(4, CubeColor.red)))

    assertEquals(parsed, expected)
  }

  //only 12 red cubes, 13 green cubes, and 14 blue cubes
  test("Validate which games are possible with a max cube set") {
    val allGames = Cube.parseAllGames(TEST_INPUT)

    val allPossibleGames = Cube.findAllPossibleGames(allGames, TEST_CUBE_MAP)

    val possibleGameIds = allPossibleGames.map(_.id)
    val expectedPossibleGameIds = List(1, 2, 5)

    assertEquals(possibleGameIds, expectedPossibleGameIds)
  }

  test("The sum of all possible games should equal 8") {
    val allGames = Cube.parseAllGames(TEST_INPUT)

    val sumOfIDs = Cube.sumIdsOfPossibleGames(allGames, TEST_CUBE_MAP)
    val expectedResult = 8

    assertEquals(sumOfIDs, expectedResult)
  }

  // 12red, 13green, 14blue
  test("Test a single game set is possible") {
    val gameSetA = GameSet(List(
      Cube(20, CubeColor.green),
      Cube(8, CubeColor.blue),
      Cube(3, CubeColor.red)
    ))

    val gameSetB = GameSet(List(
      Cube(4, CubeColor.green),
      Cube(8, CubeColor.blue),
      Cube(3, CubeColor.red)
    ))

    val isGameAValid = gameSetA.isPossible(TEST_CUBE_MAP)
    val expectedA = false

    val isGameBValid = gameSetB.isPossible(TEST_CUBE_MAP)
    val expectedB = true

    assertEquals(isGameAValid, expectedA)
    assertEquals(isGameBValid, expectedB)
  }

  test("Find the minim set of cubes for game 1: 4 red 2 green 6 blue") {
    // Game 1: 3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green
    val firstGame = Cube.parseAllGames(TEST_INPUT)
      .head

    val minimumSetOfCubes = firstGame.minimumSetOfCubes
    val expectedMinimumSetOfCubes = Map(
      CubeColor.red -> 4,
      CubeColor.green -> 2,
      CubeColor.blue -> 6
    )

    assertEquals(minimumSetOfCubes, expectedMinimumSetOfCubes)
  }

  test("Calculate the power of game 1 should equal 48") {
    val firstGame = Cube.parseAllGames(TEST_INPUT)
      .head

    val powerOfGameOne = firstGame.powerOfGame
    val expectedPower = 48

    assertEquals(powerOfGameOne, expectedPower)
  }

  test("Calculate the sum of power of games should equal 2286") {
    val games = Cube.parseAllGames(TEST_INPUT)

    val sumOfGamePowers = games
      .map(_.powerOfGame)
      .sum

    val expectedSumOfGamePower = 2286

    assertEquals(sumOfGamePowers, expectedSumOfGamePower)

  }

}
