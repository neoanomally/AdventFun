package com.sandersme.advent.twentythree.model

enum CubeColor {
  case green, red, blue
}


case class BagCubeContents(cubes: Map[CubeColor, Int]) {
  def apply(color: CubeColor): Int = {
    cubes(color)
  }
}
case class CubeGame(id: Int, gameSet: List[GameSet]) {
  def isPossible(bagCubeContents: BagCubeContents): Boolean = {
    gameSet.forall(_.isPossible(bagCubeContents))
  }

  /** This is equal to the minimum set of cubes multiplied by each other */
  def powerOfGame: Int = minimumSetOfCubes.values.product

  def minimumSetOfCubes: Map[CubeColor, Int] = {
    gameSet
      .map(_.totalColors)
      .reduce((left, right) =>
        val allKeys = left.keySet ++ right.keySet
        allKeys.map { key =>
          val max = Math.max(left.getOrElse(key, 0), right.getOrElse(key, 0))
          key -> max
        }.toMap
      )
  }
}
case class GameSet(cubes: List[Cube]) {
  def isPossible(bagCubeContents: BagCubeContents): Boolean = {
    totalColors.forall { case(color, total) =>
      val possible = bagCubeContents(color)

      total <= possible
    }
  }

  def totalColors: Map[CubeColor, Int] = {
    cubes.groupBy(_.color)
      .map { case (color, numbers) =>
        color -> numbers.map(_.value).sum
      }
  }
}
case class Cube (value: Int, color: CubeColor)

object Cube {
  // TODO
  def parseAllGames(input: List[String]): List[CubeGame] = {
    input.map(parseInput)
  }

  // For each CubeGame
  //    sum the same cube colors
  //    check if any game set per color is greater than whats in the numCubesInBag
  def findAllPossibleGames(games: List[CubeGame],
                           bagCubeContents: BagCubeContents): List[CubeGame] = {
    for {
      game <- games
      if game.isPossible(bagCubeContents)
    } yield game
  }

  def sumIdsOfPossibleGames(games: List[CubeGame],
                            bagCubeContents: BagCubeContents): Int = {
    findAllPossibleGames(games, bagCubeContents)
      .map(_.id)
      .sum
  }

  // Structure - Game 1: 3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green
  // 1. drop the first 8 characters.
  // 2. split on the semicolon ;
  // 3. map on the results and split on the space.
  //   a. Left is the value
  //   b. Right is the color
  def parseInput(input: String): CubeGame = {
    val colonIndex = input.indexOf(":")
    val gameNumber = input
      .take(colonIndex)
      .split(" ")(1)
      .trim
      .toInt

    val cubeSets = input
      .drop(colonIndex + 1)
      .split(";")
      .map(parseCubeSet)
      .toList

    CubeGame(gameNumber, cubeSets)
  }

  def parseCubeSet(set: String): GameSet = {
    val cubeSet = set
      .split(",")
      .map(_.trim)
      .map{ cube => // should be size 2
        val arr = cube.split(" ")
        val number = arr(0).toInt
        val color = CubeColor.valueOf(arr(1))

        Cube(number, color)
      }.toList

    GameSet(cubeSet)
  }
}
