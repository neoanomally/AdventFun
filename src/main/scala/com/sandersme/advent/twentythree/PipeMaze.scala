package com.sandersme.advent.twentythree

import com.sandersme.advent.Input
import com.sandersme.advent.twentythree.model.PipeGraph
import com.sandersme.advent.twentytwo.model.CommDecoder.loopIncrementCharacters

object PipeMaze extends App {
  val input = Input.readTwentyThreeFromResource("day10_input")


  val pipeGraph = PipeGraph.parseInput(input)
  val loopedMap = pipeGraph.loopThroughPipes

  val maxPoint = loopedMap.maxBy(_._2)
  println(s"The maxpoint and distance is: ${maxPoint}")

  val transformedPipeGraph = pipeGraph.transformNonMainPipesToGround
  val numInnerTiles = transformedPipeGraph.countTilesInsideLoops
  println(s"The number of tiles that are in the inner group: ${numInnerTiles}")
  transformedPipeGraph.printMainLoopWithInnerOuter
}
