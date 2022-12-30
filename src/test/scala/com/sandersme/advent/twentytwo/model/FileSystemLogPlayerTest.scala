package com.sandersme.advent.twentytwo.model

class FileSystemLogPlayerTest extends munit.FunSuite {
  val TEST_INPUT: List[String] = """$ cd /
                    |$ ls
                    |dir a
                    |14848514 b.txt
                    |8504156 c.dat
                    |dir d
                    |$ cd a
                    |$ ls
                    |dir e
                    |29116 f
                    |2557 g
                    |62596 h.lst
                    |$ cd e
                    |$ ls
                    |584 i
                    |$ cd ..
                    |$ cd ..
                    |$ cd d
                    |$ ls
                    |4060174 j
                    |8033020 d.log
                    |5626152 d.ext
                    |7214296 k""".stripMargin.split("\n").toList

  val FILE_SYSTEM_LOG_PLAYER = FileSystemLogPlayer.readAllCommands(TEST_INPUT)

  test("Validate that we can read all the commands") {
    val expectedSecondToLastCommandCd = CD("d")

    assertEquals(FILE_SYSTEM_LOG_PLAYER.commands.size, 10)
    assertEquals(FILE_SYSTEM_LOG_PLAYER.commands(8), expectedSecondToLastCommandCd)
  }

  test("Validate that we can get the current working directory. Always start at base") {
    val expectedEmptyCurrentWorkingDir = CurrentWorkingDir(List.empty)

    assertEquals(FILE_SYSTEM_LOG_PLAYER.cwd, expectedEmptyCurrentWorkingDir)

    val forwardOne = FILE_SYSTEM_LOG_PLAYER.moveForwardOneCommand
    val expectedForwardWorkingDir = CurrentWorkingDir(List("/"))

    assertEquals(forwardOne.cwd, expectedForwardWorkingDir)
  }

  test("Move forward one command and update previous commands") {
    val updatedLogPlayer = FILE_SYSTEM_LOG_PLAYER.moveForwardNCommands(2)

    val previousCommands = updatedLogPlayer.previousCommands

    assertEquals(previousCommands.size, 2)
  }

  test("Assert that the directory structure has been updated after a cd and then add") {
    // Four commands is that I have added f, gh, h and directory e to a
    val updatedLogPlayer = FILE_SYSTEM_LOG_PLAYER.moveForwardNCommands(4)

    val expectedDirA = Directory("a", List(Directory("e"), File("f", 29116L), File("g", 2557L), File("h.lst", 62596L)))
    val directoryA = updatedLogPlayer.directory.children.collectFirst {
      case dir: Directory if dir.name.equals("a") => dir
    }.get

    assertEquals(directoryA, expectedDirA)
  }

  test("Move forward all commands") {
    // Four commands is that I have added f, gh, h and directory e to a
    val updatedLogPlayer = FILE_SYSTEM_LOG_PLAYER.moveForwardAllCommands

    val rootFiles = updatedLogPlayer.directory.children.map {
      case dir: Directory =>  s"dir: ${dir.name}"
      case file: File => s"file: ${file.name}"
    }

    val expectedRootFiles = List("dir: a", "file: b.txt", "file: c.dat",  "dir: d")

    assertEquals(updatedLogPlayer.commands.isEmpty, true)
    assertEquals(updatedLogPlayer.previousCommands.length, 10)
    assertEquals(rootFiles, expectedRootFiles)
  }

  test("validate the total amount of free disk space is 21618835") {
    val updatedLogPlayer = FILE_SYSTEM_LOG_PLAYER.moveForwardAllCommands

    val freeSpace = updatedLogPlayer.freeDiskSpace
    val expectedFreeSpace = 21618835L

    assertEquals(freeSpace, expectedFreeSpace)
  }

  test("Find smallest directory to delete that will give space") {
    val updatedLogPlayer = FILE_SYSTEM_LOG_PLAYER.moveForwardAllCommands

    val fileToDelete = updatedLogPlayer.findSmallestDirectoryToDelete(30000000L)

    val expectedFileToDelete = DirectorySize("//d/", 24933642, 1)

    assertEquals(fileToDelete, expectedFileToDelete)
  }
}
