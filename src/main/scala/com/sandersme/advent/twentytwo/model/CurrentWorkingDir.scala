package com.sandersme.advent.twentytwo.model

case class CurrentWorkingDir(path: List[String]) {
  val ROOT = "/"

  def add(name: String): CurrentWorkingDir = {
    CurrentWorkingDir(path :+ name)
  }

  def :+(name: String): CurrentWorkingDir = {
    add(name)
  }

  def backOne: CurrentWorkingDir = {
    CurrentWorkingDir(path.dropRight(1))
  }
}

object CurrentWorkingDir {
  def empty: CurrentWorkingDir = CurrentWorkingDir(List.empty)
}
