package com.sandersme.advent.twentyfour

import com.sandersme.advent.twentyfour.DiskFragmenter
import scala.annotation.tailrec
import scala.collection.mutable
import com.sandersme.advent.Input


case class File(segments: Vector[Segment], freeSpace: Int) {
  def totalLength: Int = segments.map(_.length).sum
  def hasFreeSpace: Boolean = freeSpace != 0
  def hasSegments: Boolean = segments.nonEmpty

  def deleteSpace: File = { 
    File(Vector.empty, freeSpace = totalLength + freeSpace)
  }

  def addAllSegments(newSegments: Vector[Segment]): File = {
    val totalSegmentsSize = newSegments.map(_.length).sum
    if (totalSegmentsSize > freeSpace) {
      throw new Exception("Error should not reach here, but could happen")
    }

    this.copy(segments = segments ++ newSegments, freeSpace = freeSpace - totalSegmentsSize)
  }

  def addSegment(segment: Segment): (File, Option[Segment]) = {
    val updatedSpace = freeSpace - segment.length
     
    val updatedSegment = segment.copy(length = Math.min(segment.length, freeSpace))
    val updatedFile = File(segments :+ updatedSegment, Math.max(0, updatedSpace))
    val remainingSegment = if (updatedSpace < 0) {
        Some(segment.copy(length = Math.abs(updatedSpace)))
      } else {
        None
      }
    (updatedFile, remainingSegment)
  }
}
case class Segment(id: Int, length: Int) 

case class DiskProcessor(files: Vector[File], filesIdx: Int, freeSpaceIdx: Int) {

  def calculateCheckSum: BigInt = {
    files.foldLeft((BigInt(0), 0)){ case ((sum, idx), nextFile) =>
      if (!nextFile.hasSegments) {
        (sum, idx + nextFile.freeSpace)
      } else {
        nextFile.segments.foldLeft((sum, idx)) { case ((fileSum, fileIdx), segment) => 
          val segmentRes = ((0 + fileIdx) until (segment.length + fileIdx)).foldLeft(fileSum){ case (segmentSum, segmentIdx) => 
            segmentSum + (segmentIdx * segment.id)
          }

          (segmentRes, fileIdx + segment.length + nextFile.freeSpace)
        }
      }
    }._1
  }

  def calculateResultFromFinalString: BigInt = {
    val finalString = createFinalString

    val sumItr = for {
      idx <- 0 until finalString.length
      
      if(finalString(idx) != '.')
    } yield BigInt((finalString(idx).asDigit * idx))

      sumItr
        .reduceLeft(_ + _)
  }

  def createFinalString: String = {
    files.foldLeft(new mutable.StringBuilder()) { case (sb, nextFile) => 
      nextFile.segments.foreach { segment =>
        (0 until segment.length)
          .foreach(_ => sb.append(segment.id))
      }

      if (nextFile.hasFreeSpace) {
        (0 until nextFile.freeSpace).foreach{ _ => 
          sb.append('.')
        }
      }

      sb
    }.toString()
  }
  
  def findNextFreeSpace(idx: Int): (File, Int) = {
    val file = files(idx)
    if (file.hasFreeSpace) {
      (file, idx)
    } else {
      findNextFreeSpace(idx + 1)
    }
  }

  def findNextFileToMove(idx: Int): (File, Int) = {
    val file = files(idx)

    if(file.hasSegments) {
      (file, idx)
    } else {
      findNextFileToMove(idx - 1) 
    }
      
  }
}

object DiskFragmenter {
  def main(args: Array[String]): Unit = {
    val input = Input.readTwentyFourFromResource("day9_input")

    val parsedInput = DiskFragmenter.parseInput(input)
    val defragged = DiskFragmenter.defragFullDisk(parsedInput, 1)
    val checkSum = defragged.calculateCheckSum
    println("\n")
    println("THe checksum of the disk is: " + checkSum)
    val defraggedPartTwo = DiskFragmenter.defragPartTwo(parsedInput, parsedInput.files.length - 1)

    println("\n The checksum for part 2: " + defraggedPartTwo.calculateCheckSum)
  }

  @tailrec
  def defragFullDisk(diskFragmenter: DiskProcessor, numTimes: Int): DiskProcessor= {
    if (numTimes < 60000 && diskFragmenter.freeSpaceIdx < diskFragmenter.filesIdx) {
      defragFullDisk(DiskFragmenter.defragOneStep(diskFragmenter), numTimes + 1)
    } else {
      diskFragmenter
    }
  }

  def defragOneStep(diskProcessor: DiskProcessor): DiskProcessor = {
    val (fileToMove, fileIdx) = diskProcessor.findNextFileToMove(diskProcessor.filesIdx)
    val filesWithRemovedFile = diskProcessor.files.updated(fileIdx, fileToMove.deleteSpace)

    val updatedDiskProcessor = diskProcessor.copy(files = filesWithRemovedFile, filesIdx = fileIdx)

    addSegmentsToFreeSpace(updatedDiskProcessor, fileToMove.segments)
  }

  /// Todo: Add Tests
  def defragPartTwo(diskProcessor: DiskProcessor, fileToMoveIdx: Int): DiskProcessor = {
    val fileIdxOpt = findNextViableFile(diskProcessor, fileToMoveIdx)

    if (fileIdxOpt.isEmpty) {
      diskProcessor
    } else {
      val fileIdx = fileIdxOpt.get._2
      val fileLength = fileIdxOpt.get._1.totalLength
      val file = fileIdxOpt.get._1
      val freeSpaceIdxOpt = findNextViableFreeSpace(diskProcessor, 0, fileIdx, fileLength)

      val updatedProcessor = freeSpaceIdxOpt match {
        case None => diskProcessor
        case Some((freeSpaceFile, freeIdx)) => {
          val updatedFreeSpace = freeSpaceFile.addAllSegments(file.segments)
          val updatedFile = file.deleteSpace
          
          val updatedFiles = diskProcessor.files.updated(fileIdx, updatedFile)
            .updated(freeIdx, updatedFreeSpace)

          diskProcessor.copy(files = updatedFiles)
        }
      }

      defragPartTwo(updatedProcessor, fileToMoveIdx - 1)
    }
  }

  // TODO: Add tests
  private def findNextViableFreeSpace(processor: DiskProcessor, currIdx: Int,
    upperIdx: Int, space: Int): Option[(File, Int)] = {

      if (currIdx >= upperIdx) {
        None 
      } else if (processor.files(currIdx).freeSpace >= space) {
        Some((processor.files(currIdx), currIdx))
      } else {
        findNextViableFreeSpace(processor, currIdx + 1, upperIdx, space)
      }

  }

  private def findNextViableFile(diskProcessor: DiskProcessor, currIdx: Int): Option[(File, Int)] = {
    if (currIdx < 0) {
      None
    } else if (diskProcessor.files(currIdx).hasSegments) {
      Some((diskProcessor.files(currIdx), currIdx))
    } else {
      findNextViableFile(diskProcessor, currIdx - 1)
    }
  }
  
  def addSegmentsToFreeSpace(diskProcesser: DiskProcessor, segments: Vector[Segment]): DiskProcessor = {
    if (segments.isEmpty) {
      diskProcesser
    } else {
      val segment = segments.head
      val (nextFreeSpaceFile, updatedFreeSpaceIdx) = diskProcesser.findNextFreeSpace(diskProcesser.freeSpaceIdx)

      val (updatedFile, remainingSegment) = diskProcesser.files(updatedFreeSpaceIdx).addSegment(segment)
     
      val remainingSegments = remainingSegment match {
        case None => segments.tail
        case Some(moreSegment) => moreSegment +: segments.tail
      }

      val updatedFiles = diskProcesser.files.updated(updatedFreeSpaceIdx, updatedFile)
      val updatedDiskProcessor = diskProcesser.copy(files = updatedFiles, freeSpaceIdx = updatedFreeSpaceIdx)
        addSegmentsToFreeSpace(updatedDiskProcessor, remainingSegments)
    }
  }



  def parseInput(in: List[String]): DiskProcessor = {
    val (allFiles, _, _) = in.head
      .map(_.asDigit)
      .foldLeft((Vector.empty[File], true, 0)){ case ((files, isOdd, id), length) =>
        val updatedFiles = if (isOdd) {
          files :+ File(Vector(Segment(id, length)), 0)
        } else {
          files :+ File(Vector.empty, length)
        }

        val updatedId = if (isOdd) { id + 1 } else { id }

        (updatedFiles, !isOdd, updatedId)
      }

    DiskProcessor(allFiles, allFiles.length - 1, 1)
  }
}
