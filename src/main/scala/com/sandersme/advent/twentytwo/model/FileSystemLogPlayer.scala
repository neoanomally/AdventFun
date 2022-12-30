package com.sandersme.advent.twentytwo.model

import com.sandersme.advent.twentytwo.model.FileSystemLogPlayer.addFilesToDirectory

import scala.annotation.tailrec

case class FileSystemLogPlayer(commands: List[FSCommand],
                               directory: Directory = Directory("/"),
                               previousCommands: List[FSCommand] = List.empty,
                               cwd: CurrentWorkingDir = CurrentWorkingDir.empty) {

  val TOTAL_DISK_SPACE = 70000000

  def freeDiskSpace: Long = {
    val directorySizes = Directory.collectDirectorySizes(directory)

    val rootSize = directorySizes.find(_.name == "//").get

    TOTAL_DISK_SPACE - rootSize.size
  }

  def findSmallestDirectoryToDelete(totalSpaceNeeded: Long): DirectorySize = {
    val directorySize = Directory.collectDirectorySizes(directory)
    val spaceNeeded = totalSpaceNeeded - freeDiskSpace

    val results: DirectorySize = directorySize
      .map(dirSize => (dirSize, dirSize.size - spaceNeeded))
      .filter(_._2 > 0)
      .minBy(_._2)
      ._1

    results
  }

  def moveForwardNCommands(n: Int): FileSystemLogPlayer = {
    (1 to n).foldLeft(this)((fileSystemLogPlayer, _) => fileSystemLogPlayer.moveForwardOneCommand)
  }

  def moveForwardAllCommands: FileSystemLogPlayer = {
    FileSystemLogPlayer
      .moveForwardAllCommands(this)
  }

  def moveForwardOneCommand: FileSystemLogPlayer = {
    val nextCommand = commands.headOption
    val remainingCommands = commands.tail
    val updatedPreviousCommands = previousCommands :+ nextCommand.getOrElse(InvalidCommand)

    nextCommand match {
      case None => this
      case Some(CD(name)) if name == ".." =>
        this.copy(commands = remainingCommands, previousCommands = updatedPreviousCommands,
          cwd = cwd.backOne)
      case Some(CD(name)) =>
        this.copy(commands = remainingCommands,
          previousCommands = updatedPreviousCommands,
          cwd = cwd.add(name))
      case Some(LS(files)) =>
        val updatedLogDirectory = FileSystemLogPlayer.addFilesToDirectory(this, files)
        this.copy(commands = remainingCommands, previousCommands = updatedPreviousCommands,
          directory = updatedLogDirectory)
      case Some(InvalidCommand) => this
    }
  }
}
object FileSystemLogPlayer {
  @tailrec
  private def moveForwardAllCommands(fileSystemLogPlayer: FileSystemLogPlayer): FileSystemLogPlayer = {
    if (fileSystemLogPlayer.commands.isEmpty) {
      fileSystemLogPlayer
    } else {
      moveForwardAllCommands(fileSystemLogPlayer.moveForwardOneCommand)
    }
  }

  def readAllCommands(line: List[String]): FileSystemLogPlayer = {
    val fsCommands = FSCommand.parseAllFSCommands(line)
    FileSystemLogPlayer(fsCommands)
  }

  /** With the current working directory add all files to the directory structure.  */
  private def addFilesToDirectory(logPlayer: FileSystemLogPlayer,
                                  files: List[FileType]): Directory = {
      Directory.findAndUpdateDirectory(logPlayer.directory, files, logPlayer.cwd.path.tail)
  }

}
