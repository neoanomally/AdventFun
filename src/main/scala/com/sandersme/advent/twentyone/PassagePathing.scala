package com.sandersme.advent.twentyone

import com.sandersme.advent.Input
import com.sandersme.advent.twentyone.model.PathFinder

object PassagePathing {

  /**
   * * With your submarine's subterranean subsystems subsisting suboptimally, the only
   *   way you're getting out of this cave anytime soon is by finding a path yourself.
   *   Not just a path - the only way to know if you've found the best path is to find
   *   all of them.
   *
   *   Fortunately, the sensors are still mostly working, and so you build a rough map
   *   of the remaining caves (your puzzle input). For example:
   *
   *   start-A
   *   start-b
   *   A-c
   *   A-b
   *   b-d
   *   A-end
   *   b-end
   *   This is a list of how all of the caves are connected. You start in the cave
   *   named start, and your destination is the cave named end. An entry like b-d
   *   means that cave b is connected to cave d - that is, you can move between them.
   *
   *   So, the above cave system looks roughly like this:
   *
   *       start
   *       /   \
   *   c--A-----b--d
   *       \   /
   *        end Your goal is to find the number of distinct paths that start at
   *       start, end at end, and don't visit small caves more than once. There are
   *       two types of caves: big caves (written in uppercase, like A) and small
   *       caves (written in lowercase, like b). It would be a waste of time to
   *       visit any small cave more than once, but big caves are large enough that
   *       it might be worth visiting them multiple times. So, all paths you find
   *       should visit small caves at most once, and can visit big caves any
   *       number of times.
   *
   *       After reviewing the available paths, you realize you might have time to visit
   *       a single small cave twice. Specifically, big caves can be visited any number of
   *       times, a single small cave can be visited at most twice, and the remaining small
   *       caves can be visited at most once. However, the caves named start and end can only
   *       be visited exactly once each: once you leave the start cave, you may not return to
   *       it, and once you reach the end cave, the path must end immediately.


   * @param args
   */
  def main(args: Array[String]): Unit = {
    val input = Input.readTwentyOneFromResource("day12_input")

    val pathMap = PathFinder.parseInput(input)

    val findAllPaths = PathFinder.findPathsToEnd(pathMap)

    println(s"The number of paths from start to end visiting" +
      s"only small caves twice once: ${findAllPaths.size}")
  }

}
