package com.sandersme.advent

import com.sandersme.advent.twentyone.model.{Direction, PilotCommand}

import scala.io.Source

object Input {
  def readFromDataResource(directory: String, filename: String): List[String] = {
    val path = s"data/${directory}/${filename}"
    val file = Source.fromResource(path, Input.getClass.getClassLoader)
    val lines = file.getLines().toList
    
    file.close()
    lines
  }

  def readTwentyOneFromResource(filename: String): List[String] = {
    readFromDataResource("twentyone", filename)
  }

  def readTwentyTwoFromResource(filename: String): List[String] = {
    readFromDataResource("twentytwo", filename)
  }

  def readTwentyThreeFromResource(filename: String): List[String] = {
    readFromDataResource("twentythree", filename)
  }

  def readTwentyFourFromResource(filename: String): List[String] = {
    readFromDataResource("twentyfour", filename)
  }

  // TODO MOve this somewhere else
  def parseDay2Input(line: String): PilotCommand = {

    val split = line.split(" ")

    val direction = Direction.valueOf(split(0).toLowerCase.capitalize)
    val unit = split(1).toInt

    PilotCommand(direction, unit)
  }
}
