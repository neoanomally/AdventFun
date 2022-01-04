package com.sandersme.advent.twentyone

import com.sandersme.advent.twentyone.model.DumboGrid

/**
 * * You enter a large cavern full of rare bioluminescent dumbo octopuses! They seem
 * to not like the Christmas lights on your submarine, so you turn them off for
 * now.
 *
 * There are 100 octopuses arranged neatly in a 10 by 10 grid. Each octopus slowly
 * gains energy over time and flashes brightly for a moment when its energy is
 * full. Although your lights are off, maybe you could navigate through the cave
 * without disturbing the octopuses if you could predict when the flashes of light
 * will happen.
 *
 * Each octopus has an energy level - your submarine can remotely measure the
 * energy level of each octopus (your puzzle input). For example:
 *
 * 5483143223
 * 2745854711
 * 5264556173
 * 6141336146
 * 6357385478
 * 4167524645
 * 2176841721
 * 6882881134
 * 4846848554
 * 5283751526
 * The energy level of each octopus is a value between 0 and 9. Here, the top-left
 * octopus has an energy level of 5, the bottom-right one has an energy level of
 * 6, and so on.
 *
 * You can model the energy levels and flashes of light in steps. During a single step, the following occurs:
 *
 * First, the energy level of each octopus increases by 1. Then, any octopus with
 * an energy level greater than 9 flashes. This increases the energy level of
 * all adjacent octopuses by 1, including octopuses that are diagonally
 * adjacent. If this causes an octopus to have an energy level greater than 9,
 * it also flashes. This process continues as long as new octopuses keep having
 * their energy level increased beyond 9. (An octopus can only flash at most
 * once per step.) Finally, any octopus that flashed during this step has its
 * energy level set to 0, as it used all of its energy to flash. Adjacent
 * flashes can cause an octopus to flash on a step even if it begins that step
 * with very little energy. Consider the middle octopus with 1 energy in this
 * situation:
 *
 * After 100 steps, there have been a total of 1656 flashes.
 */
object DumboOctopus {
  def main(args: Array[String]): Unit = {
    val input = Input.readFromDataResource("day11_input")
    val dumboGrid = DumboGrid.parseInput(input)

    val dumbGrid100Steps = (0 to 100).foldLeft(dumboGrid){ case (grid, _) =>
      DumboGrid.energyIncrease(grid)
    }

    val dumboGridFindStepAllOctopusFlash = DumboGrid.findStepAllOctopusFlash(dumboGrid)

    println(s"The total number of flashes after 100 steps: ${dumbGrid100Steps.totalNumberFlashes}")
    println(s"Total number of steps till all octopus" +
      s"flash: ${dumboGridFindStepAllOctopusFlash.stepAllFlashed}")
  }
}
