package com.sandersme.advent.twentytwo

import com.sandersme.advent.Input
import com.sandersme.advent.twentytwo.model.{Directory, FileSystemLogPlayer}

object NoSpaceLeftOnDevice {

  /**
   * You can hear birds chirping and raindrops hitting leaves as the expedition proceeds. Occasionally, you can even hear
   * much louder sounds in the distance; how big do the animals get out here, anyway?
   * The device the Elves gave you has problems with more than just its communication system. You try to run a system
   * update:
   *
   * $ system-update --please --pretty-please-with-sugar-on-top
   * Error: No space left on device
   *
   * Perhaps you can delete some files to make space for the update?
   * You browse around the filesystem to assess the situation and save the resulting terminal output
   * @param args
   */
  def main(args: Array[String]): Unit = {

    val input = Input.readTwentyTwoFromResource("day7_input")

    val fileSystemLogPlayer = FileSystemLogPlayer.readAllCommands(input)
    val forwardAllCommands = fileSystemLogPlayer.moveForwardAllCommands

    val sumOfDirectoriesAtMost100k = Directory.sumDirectoriesAtMostN(forwardAllCommands.directory, 100000L)

    println(s"Sum of directories that are at most 100000L in length: ${sumOfDirectoriesAtMost100k}")

    Directory.printDirectorySizes(forwardAllCommands.directory)

    val expectedFileToDelete  = forwardAllCommands.findSmallestDirectoryToDelete(30000000L)

    println(s"Expected File to Delete: ${expectedFileToDelete}")
  }
}
