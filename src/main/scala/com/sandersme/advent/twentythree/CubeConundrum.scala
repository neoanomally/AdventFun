package com.sandersme.advent.twentythree

import com.sandersme.advent.Input
import com.sandersme.advent.twentythree.model.{BagCubeContents, Cube, CubeColor}

object CubeConundrum {

  def main(args: Array[String]): Unit = {
    val input = Input.readTwentyThreeFromResource("day2_input")

    val games = Cube.parseAllGames(input)

    val bagCubeContents = BagCubeContents(Map(
      CubeColor.red -> 12,
      CubeColor.blue -> 14,
      CubeColor.green -> 13
    ))

     
    val sumOfPossibleCubes = Cube.sumIdsOfPossibleGames(games, bagCubeContents)

    println(s" The sum of all possible game cube IDs: ${sumOfPossibleCubes}")

    val sumOfGamePowers = games
      .map(_.powerOfGame)
      .sum

    println(s"The sum of the power of all games: ${sumOfGamePowers}")
  }

}
