package com.sandersme.advent.twentyone.model

import  com.sandersme.advent.twentyone.model.MinMaxDimensions
import com.sandersme.advent.twentyone.binary._
import scala.annotation.tailrec


type ImageLocation = (Int, Int)
type ImageAlgorithm = Vector[Boolean]

case class ImageEnhancement(activePixels: Set[ImageLocation], algorithm: ImageAlgorithm,
                            iteration: Int = 0) {
  def activePixelCount: Int = activePixels.size

  def isBorderActive: Boolean = {
    algorithm(0) && iteration % 2 != 0
  }

  lazy val borderDimensions: MinMaxDimensions = {
    val defaultMinMax: MinMaxDimensions = MinMaxDimensions.default

    val minMaxDimensions: MinMaxDimensions = activePixels
      .foldLeft(defaultMinMax){ case (minMax, location: ImageLocation) =>
      MinMaxDimensions(Math.min(minMax.minX, location._1), Math.min(minMax.minY, location._2),
        Math.max(minMax.maxX, location._1), Math.max(minMax.maxY, location._2))
    }

    minMaxDimensions
  }

  /**
   * For a given grab the algorithm pixel boolean. The index should be between
   * 0 and 511 (2^^9
   * @param index
   * @return
   */
  private[model] def algorithmLookup(index: Int): Boolean = {
    if (index < algorithm.size && index >= 0)
      algorithm(index)
    else
      false
  }

  /**
   * Checks to see if the pixel is active OR if the border is active and
   * the value is beyond the border.
   * @param x
   * @param y
   * @return
   */
  private[model] def isActive(x: Int, y: Int): Bit = {
    if (activePixels.contains((x, y))) {
      One
    } else if (isBorderActive && isBeyondBorder(x, y)) {
      One
    } else {
      Zero
    }
  }

  /** Checks to see if the pixel is on or beyond the border. This is useful
   * for when the starting pixels are active.
   */
  private[model] def isBeyondBorder(x: Int, y:Int): Boolean = {
    x < borderDimensions.minX || y < borderDimensions.minY ||
      x > borderDimensions.maxX || y > borderDimensions.maxY
  }

  def applyEnhancementAlgorithm(numTimes: Int = 1): ImageEnhancement = {
    ImageEnhancement.applyEnhancementAlgorithmXTimes(numTimes, this)
  }

  def printGrid: Unit = {
    (borderDimensions.minY - 1 to borderDimensions.maxY + 1).foreach{ y =>
      (borderDimensions.minX - 1 to borderDimensions.maxX + 1).foreach{ x =>
       val character = isActive(x, y) match {
         case One => '#'
         case Zero => '.'
       }

       print(character)
      }
      println()
    }

  }
}

object ImageEnhancement {


  /**
   * TODO: After looking at the input every ODD FLIP the infinite space becomes pixelized. THEN the 512, if all
   * the neighbors are pixelated they will turn off.
   * // SO ON THE EVEN STEPS EVERY ITEM AROUND THE EDGE +3 from min and MAX: will be on.
   * // Those pixels need to be added.
   * ESSENTIALLy
   * @param x
   * @param imageEnhancement
   * @return
   */
  @tailrec
  private[model] def applyEnhancementAlgorithmXTimes(x: Int, imageEnhancement: ImageEnhancement): ImageEnhancement = {
    if (x <= 0) {
      imageEnhancement
    } else {
      val border = imageEnhancement.borderDimensions
      val modifier = if (imageEnhancement.isBorderActive) 2 else 1

      val updatedActivePixels: Seq[(Int, Int)] = for {
        x <- border.minX - modifier to border.maxX + modifier
        y <- border.minY - modifier to border.maxY + modifier

        potentialPixel <- updateImageLocation((x, y), imageEnhancement)
      } yield potentialPixel

      val updatedImageEnhancement = imageEnhancement.copy(activePixels = updatedActivePixels.toSet,
        iteration = imageEnhancement.iteration + 1)

      applyEnhancementAlgorithmXTimes(x - 1, updatedImageEnhancement)
    }
  }

  /**
   * The first line in the file is the enhancement Algorithm.
   * Then skip the first two lines and use the grid coordinates to get the set
   * of lit pixels. At first it creates a  full grid, but then it filters out so we only
   * have the active pixel coordinates.
   * @param input
   * @return
   */
  def parseInput(input: List[String]): ImageEnhancement = {
    val algorithm = parseEnhancementAlgorithm(input.head)

    val initialPixelGrid = input.drop(2) // Drop the enhancement and the extra space
      .zipWithIndex.flatMap{ case (row, y) =>
        row.zipWithIndex.map{ case (value, x) =>
          val pixelLit = value match {
            case '#' => true
            case _ => false
          }

          (x, y) -> pixelLit
        }
      }

    val activePixes = initialPixelGrid.filter(_._2).map(_._1).toSet
    ImageEnhancement(activePixes, algorithm)
  }

  /**
   * This parses the header which contains '.' and '#' characters to represent off and on
   * respectively.
   *
   * @param inputAlgorithm This is the header String.
   * @return A Vector of booleans. This will be used upstream to create PixelLocations
   */
  private[model] def parseEnhancementAlgorithm(inputAlgorithm: String): Vector[Boolean] = {
    inputAlgorithm.map {
      case '#' => true
      case _ => false
    }.toVector
  }

  /**
   * This might actually be a dumb way to do it. Right now it gets ALL potential. This could
   * become really huge in memory. It might be more performant to do batches of these so that
   * memory doesn't blow up too quickly.  THE downside of that is we'd have to do more
   * computations for the updateImageLocation
   * @param imageEnhancement
   * @return
   */
  private[model] def getAllActiveNeighbors(imageEnhancement:
                                           ImageEnhancement): Set[ImageLocation] = {
    imageEnhancement.activePixels
      .foldLeft(Set.empty[ImageLocation]){ case (accum, imageLocation) =>
        val neighbors = getLocationPotentialNeighbors(imageLocation)

        accum ++ neighbors
      }
  }


  /**
   * Todo this will take in a single location and create a range of locations, but what
   * we really care about ahead of time are all the centroids that may be active. So the
   * center needs to touch an active pixel.
   * One of the issues is that we may have duplicates from another location.
   * So we may want to keep track of locations we have already processed which
   * in that case we'd enter in a Set here. What we really want is the
   * center location for each range. Need to reread to verify
   * THIS Is a brute force way to get all potential neighbors, INSTEAD what I can do
   * is have a SLIDING 3x3 grid that starts at the upper left most part of the image
   * then the lower right side of the image
   * @param location
   * @return
   */
  def getLocationPotentialNeighbors(location: ImageLocation): List[ImageLocation] = {
    getAllNeighborLocations(location, 2)
  }

  /**
   * Get the binary values based on the location. Using the center it will construct the 9-bit binary
   * value and convert it to an index by calling the GetAlgorithmIndex.
   * This is has an operation cost of 10, we need to get the algorithmIndex which does 9 checks
   * in the enhancementSet. Then we need to convert the binary which has some math costs
   * then we need to lookup the index. So total it's probably 12 operations, which isn't bad
   * but we are going to do this for each found pixel.
   *
   * Then it does a lookup in our 512 location
   * @param location
   * @param enhancement
   * @return
   */
  private[model] def updateImageLocation(location: ImageLocation,
                                         enhancement: ImageEnhancement): Option[ImageLocation] = {
    val index = getAlgorithmIndex(location, enhancement)
    if (enhancement.algorithmLookup(index)) {
      Some(location)
    } else {
      None
    }
  }

  /**
   * This is just to get the immediate neighbors surrounding a center location.
   * @param location
   * @return
   */
  private[model] def getImmediateNeighbors(location: ImageLocation): List[ImageLocation] = {
    getAllNeighborLocations(location, 1)
  }

  /**
   * These is a helper method that lets us get our neighbors from a specific span. We need this
   * because the lookup algorithm looks at immediate neighbors of the center location
   * and the zoom enhancement looks at a span of 2
   * @param location centroid that we are looking at neighbors for.
   * @return List of all neighbors within a span
   */
  private[model] def getAllNeighborLocations(location: ImageLocation, span: Int): List[ImageLocation] = {
    val locations = for {
      yIncr <- -span to span
      xIncr <- -span to span

      x = location._1 + xIncr
      y = location._2 + yIncr
    } yield (x, y)

    locations.toList
  }

  /**
   *
  *  Using the Center pixel, what we need to do is get the boolean value
   * of the left top corner, I can test this by starting with zero zero and making sure it's
   * in this orientation
   * (-1, -1), (0, -1), (1, -1), (-1, 0), (0, 0), (1, 0), (-1, 1), (0, 1), (1, 1)
   * @param location: The location of the pixel that we want to see the indexValue
   * @return The integer value from the binary created from surrounding pixels
   */
  private[model] def getAlgorithmIndex(location: ImageLocation,
                                       imageEnhancement: ImageEnhancement): Int = {
    val bits = getImmediateNeighbors(location)
      .map{ case(x, y) => imageEnhancement.isActive(x, y) }

    Bits.toInt(bits.toVector)
  }
}
