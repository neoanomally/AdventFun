package com.sandersme.advent.twentythree

import com.sandersme.advent.Input
import com.sandersme.advent.twentythree.model.GalaxyImage

object CosmicExpansion extends App {
  val input = Input.readTwentyThreeFromResource("day11_input")
  val galaxy = GalaxyImage.parseInput(input)
  val expandedGalaxy = galaxy.expandGalaxy()

  val summedDistance = expandedGalaxy.sumDistanceEveryPair
  println("The sum of distances for the expanded galaxy is equal to: " + summedDistance)

  val summedDistanceMillion = galaxy.expandGalaxy(1000000).sumDistanceEveryPair
  println(s"THe sum of distances for an expanded galaxy 1,000,000 times larger: ${summedDistanceMillion}")
}
