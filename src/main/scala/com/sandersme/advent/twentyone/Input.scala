package com.sandersme.advent.twentyone

import com.sandersme.advent.twentyone.model.PilotCommand
import com.sandersme.advent.twentyone.model.Direction

import scala.io.Source

object Input {
  def readFromResource(filename: String): List[String] = {
    val file = Source.fromResource(filename, Input.getClass.getClassLoader)
    val lines = file.getLines().toList
    file.close()
    lines
  }

  def readFromDataResource(filename: String): List[String] = {
    readFromResource(s"data/$filename")
  }

  // TODO MOve this somewhere else
  def parseDay2Input(line: String): PilotCommand = {

    val split = line.split(" ")

    val direction = Direction.valueOf(split(0).toLowerCase.capitalize)
    val unit = split(1).toInt

    PilotCommand(direction, unit)
  }
}
