package com.sandersme.advent.twentytwo.model

import scala.annotation.tailrec

sealed trait FileType
case class Directory(name: String, children: List[FileType] = List.empty) extends FileType
case class DirectorySize(name: String, size: Long, ident: Int)
case class File(name: String, size: Long) extends FileType

object Directory {
  // TODO: Rewrite this so that it can be tail recursive. I think the only way to do this is by
  // returning only the children of the directory instead of the updated directory. Then the base
  // Condition will be copying the final directory all the way down?
  def findAndUpdateDirectory(dir: Directory, files: List[FileType],
                                     workingDir: List[String]): Directory = {
    // This is the directory to update and might be empty
    if (workingDir.length < 1) {
      dir.copy(children = dir.children ++ files)
    } else {
      val directoryToFind = workingDir.head

      val nextDirIdx: Int = dir.children
        .indexWhere {
          case dir: Directory => dir.name.equals(directoryToFind)
          case _ => false
        }

      val nextDir = dir.children(nextDirIdx).asInstanceOf[Directory]
      val updatedChildDir: Directory = findAndUpdateDirectory(nextDir, files, workingDir.tail)
      val finalChildren = dir.children.updated(nextDirIdx, updatedChildDir)

      dir.copy(children = finalChildren)
    }
  }

  def collectChildrenFiles(directory: Directory): List[File] = {
    directory.children.collect {
      case f: File => f
    }
  }

  def collectChildrenDirs(directory: Directory): List[Directory] = {
    directory.children.collect {
      case d: Directory => d
    }
  }

  def gatherAllDirectories(directory: Directory): List[Directory] = {
    val childrenDirectories = Directory.collectChildrenDirs(directory)

    if (childrenDirectories.isEmpty) {
      List(directory)
    } else {
      directory :: childrenDirectories.flatMap(gatherAllDirectories)
    }
  }

  def gatherAllFilesFromDirectory(directory: Directory): List[File] = {
    val childrenDirectories: List[Directory] = Directory.collectChildrenDirs(directory)
    val files: List[File] = Directory.collectChildrenFiles(directory)

    if(childrenDirectories.isEmpty) {
      files
    } else {
      files ++ childrenDirectories.flatMap(gatherAllFilesFromDirectory)
    }
  }

  // Find child leaves=
  private def recurseFileSizes(directories: List[Directory]): Long = {
    val childrenFiles = directories.map(Directory.collectChildrenFiles)
    val childrenDirectories = directories.flatMap(Directory.collectChildrenDirs)
    val childrenFileSizes = childrenFiles.map(_.size).sum

    if (childrenDirectories.isEmpty) {
      childrenFileSizes
    } else {
      childrenFileSizes + recurseFileSizes(childrenDirectories)
    }
  }

  // TODO: THis is double counting figure it out stupuid.
  def collectFileSizes(directory: Directory, indent: Int = 0, parent: String = ""): List[DirectorySize] = {
    val pathName = s"${parent}${directory.name}/"

    val all: List[DirectorySize] = directory.children.collect {
      case d: Directory => collectFileSizes(d, indent + 1, pathName)
      case f: File => List(DirectorySize(pathName + f.name, f.size, indent + 1))
    }.flatten

    DirectorySize(pathName.dropRight(1), all.map(_.size).sum, indent) :: all
  }

  // TODO: Not really a todo but i don't like how I have all the forward slashes and names.
  def collectDirectorySizes(directory: Directory, indent: Int = 0,
                            path: String = ""): List[DirectorySize] = {
    val fileSizesSum: Long = collectChildrenFiles(directory).map(_.size).sum
    val childrenDirs: List[Directory] = collectChildrenDirs(directory)
    val namePath = s"${path}${directory.name}/"

    val immediateChildrenNameSet = childrenDirs
      .map(childName => s"${namePath}${childName.name}/")
      .toSet

    val childrenDirectorySizes = childrenDirs
      .flatMap(dir => collectDirectorySizes(dir, indent + 1, namePath))

    val immediateChildrenSizeSum = childrenDirectorySizes
      .filter(dir => immediateChildrenNameSet.contains(dir.name))
      .map(_.size)
      .sum

    val totalSizes: Long = fileSizesSum + immediateChildrenSizeSum

    DirectorySize(namePath, totalSizes, indent) +: childrenDirectorySizes
  }

  def printDirectorySizes(directory: Directory): Unit = {
    val collected = collectDirectorySizes(directory)

    collected.map{ directorySize =>
      val prefix = {List.fill(directorySize.ident * 2)(" ").mkString}

      s"${prefix} - ${directorySize.name} size: ${directorySize.size}"
    }.foreach(println)
  }

  def sumDirectoriesAtMostN(directory: Directory, atMost: Long): Long = {
    collectDirectorySizes(directory)
      .filter(_.size < atMost)
      .map(_.size)
      .sum
  }
}

object FileType {
  def parseFiletype(input: String): FileType = {
    val split = input.split(" ")
    assert(split.length == 2)

    val first = split.head
    val name = split(1)

    if (input.startsWith("dir")) {
      Directory(name)
    } else {
      File(name, first.toLong)
    }
  }
}

