package com.sandersme.advent.twentytwo.model

class DirectoryTest extends munit.FunSuite {
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

  val FILESYSTEM_LOG_PLAYER: FileSystemLogPlayer = FileSystemLogPlayer.readAllCommands(TEST_INPUT)
  // Four commands is that I have added f, gh, h and directory e to a
  val UPDATED_LOG_PLAYER: FileSystemLogPlayer = FILESYSTEM_LOG_PLAYER.moveForwardAllCommands


  test("Test summing up all directories where their total size is at most 100000 should equal 95437") {
    val finalSize = Directory.sumDirectoriesAtMostN(UPDATED_LOG_PLAYER.directory, 100000L)

    assertEquals(finalSize, 95437L)
  }

  test("Gather All Directories") {
    val directory = FILESYSTEM_LOG_PLAYER.moveForwardAllCommands.directory
    val allDirectories = Directory.gatherAllDirectories(directory)

    val results = allDirectories.map { dir =>
      val allFiles = Directory.gatherAllFilesFromDirectory(dir)
      val totalSize: Long = allFiles.map(_.size).sum

      dir.name -> totalSize
    }

    val expectedResults: List[(String, Long)] = List("/" -> 48381165L, "a" -> 94853, "e" -> 584, "d" -> 24933642L)

    assertEquals(results, expectedResults)
  }


  test("Collect directory sizes") {
    val collectedSizes = Directory.collectDirectorySizes(UPDATED_LOG_PLAYER.directory)

    val sizes: List[Long] = collectedSizes.map(_.size)
    val expectedSizes: List[Long] = List(48381165,  94853, 584, 24933642)

    assertEquals(sizes, expectedSizes)
  }

}
