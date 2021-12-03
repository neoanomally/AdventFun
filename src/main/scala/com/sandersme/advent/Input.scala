package com.sandersme.advent

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
}
