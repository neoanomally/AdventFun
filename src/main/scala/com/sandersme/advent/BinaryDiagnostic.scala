package com.sandersme.advent

object BinaryDiagnostic {

  sealed trait Bit
  object One extends Bit
  object Zero extends Bit

  case class OxygenGenerator(rating: Int)
  case class C02Scrubber(rating: Int)


  case class BinaryCoding(bits: List[Bit]) {
    override def toString: String = {
      bits.map(bit => bit match {
        case Zero => "0"
        case One => "1"
      }).reduce(_ + _)
    }

    def toInt: Int = {
      Integer.parseInt(toString, 2)
    }
  }

  case class BitTypeCounters(counters: List[BitTypeCounter]) {
    def gammaRate: BinaryCoding = {
      val bits = counters.map { bitTypeCounter =>
        if (bitTypeCounter.oneCount >= bitTypeCounter.zeroCount) {
          One
        } else {
          Zero
        }
      }

      BinaryCoding(bits)
    }

    def epsilonRate: BinaryCoding = {
      val bits = gammaRate.bits.map{ bit =>
        bit match {
          case One => Zero
          case Zero => One
        }
      }

      BinaryCoding(bits)
    }

    def powerConsumption: Int = {
      epsilonRate *= gammaRate
    }
  }
  case class BitTypeCounter(zeroCount: Int, oneCount: Int) {
    def += (thatBitCounter: BitTypeCounter): BitTypeCounter = {
      val zeroes = zeroCount + thatBitCounter.zeroCount
      val ones = oneCount + thatBitCounter.oneCount

      BitTypeCounter(zeroes, ones)
    }
  }

  object BitTypeCounter {
    val DEFAULT_BIT_COUNTER = BitTypeCounter(0, 0)
  }

  object BinaryCoding {
    def apply(input: String): BinaryCoding = {
      val bits = input.toList
        .map(value => if(value == '1') One else Zero)

      BinaryCoding(bits)
    }

    extension (bitCoding: BinaryCoding) {
      def toBitCounters: BitTypeCounters = {
        val counters: List[BitTypeCounter] = bitCoding
          .bits
          .map(bit => bit match {
            case Zero => BitTypeCounter(1, 0)
            case One => BitTypeCounter(0, 1)
          })

        BitTypeCounters(counters)
      }

      def *=(other: BinaryCoding): Int = {
        bitCoding.toInt * other.toInt
      }
    }

    /**
     * Map the bitCodings into BitCounters
     * Combine two rows of BitCounters
     * Then add each of the columns together.
     *
     * TODO Instead of returning a List of BitTypeCounter we should have a container
     * class to indicate that the int are variable fields.
     */
    extension (binaryCodingList: List[BinaryCoding])
      def sumBinaryCodingColumns: BitTypeCounters = {
          val counters = binaryCodingList
            .map(_.toBitCounters.counters)
            .reduce{ case (leftCounterList, rightCounterList) =>
              val zippedCounterList = leftCounterList zip rightCounterList
              zippedCounterList
                .map{ case(leftBitCounter, rightBitCounter) =>
                  leftBitCounter += rightBitCounter
                }
            }

          BitTypeCounters(counters)
        }

    /**
     * Each bit in the gamma rate can be determined by finding the most common bit in the corresponding
     * position of all numbers in the diagnostic report.
     * Each bit in the episolon rate can be determined by finding the least common bit in the corresponding
     * position of all numbers in the diagnostic report.
     *
     * // TODO FIGURE OUT WHAT TO DO WHEN THEY ARE EQUAL. FOR NOW WE'll have them members of Both
     */
  }

  /**
   * The submarine has been making some odd creaking noises, so you ask it to produce a diagnostic report
   * just in case.
   * The diagnostic report (your puzzle input) consists of a list of binary numbers which, when decoded properly,
   * can tell you many useful things about the conditions of the submarine. The first parameter to check is the power
   * consumption.
   * You need to use the binary numbers in the diagnostic report to generate two new binary numbers (called the gamma
   * rate and the epsilon rate). The power consumption can then be found by multiplying the gamma rate
   * by the epsilon rate.
   * Each bit in the gamma rate can be determined by finding the most common bit in the corresponding position of
   * all numbers in the diagnostic report. For example, given the following diagnostic report:
   *
   * Considering only the first bit of each number, there are five 0 bits and seven 1 bits. Since the most common bit is 1, the first bit of the gamma rate is 1.
   * The most common second bit of the numbers in the diagnostic report is 0, so the second bit of the gamma rate is 0.
   * The most common value of the third, fourth, and fifth bits are 1, 1, and 0, respectively, and so the
   * final three bits of the gamma rate are 110.
   * So, the gamma rate is the binary number 10110, or 22 in decimal.
   * The epsilon rate is calculated in a similar way; rather than use the most common bit, the least common bit
   * from each position is used. So, the epsilon rate is 01001, or 9 in decimal. Multiplying the gamma rate (22) by
   * the epsilon rate (9) produces the power consumption, 198.
   * Use the binary numbers in your diagnostic report to calculate the gamma rate and epsilon rate, then multiply
   * them together. What is the power consumption of the submarine? (Be sure to represent your answer in
   * decimal, not binary.)
   *
   * ---------- PART TWO ------------------
   * Next, you should verify the life support rating, which can be determined by multiplying the oxygen
   * generator rating by the CO2 scrubber rating.
   * Both the oxygen generator rating and the CO2 scrubber rating are values that can be found in your diagnostic
   * report - finding them is the tricky part. Both values are located using a similar process that involves filtering
   * out values until only one remains. Before searching for either rating value, start with the full list of binary
   * numbers from your diagnostic report and consider just the first bit of those numbers. Then:
   * Keep only numbers selected by the bit criteria for the type of rating value for which you are searching.
   * Discard numbers which do not match the bit criteria.
   * If you only have one number left, stop; this is the rating value for which you are searching.
   * Otherwise, repeat the process, considering the next bit to the right.
   * The bit criteria depends on which type of rating value you want to find:

To find oxygen generator rating, determine the most common value (0 or 1) in the current bit position, and keep only numbers with that bit in that position. If 0 and 1 are equally common, keep values with a 1 in the position being considered.
To find CO2 scrubber rating, determine the least common value (0 or 1) in the current bit position, and keep only numbers with that bit in that position. If 0 and 1 are equally common, keep values with a 0 in the position being considered.
   */
  def main(args: Array[String]): Unit = {
    import BinaryCoding._

    val binaryInput = Input.readFromDataResource("day3_input")

    val bitTypeCounters = binaryInput
      .map(BinaryCoding.apply)
      .sumBinaryCodingColumns

    val gammRate = bitTypeCounters.gammaRate
    val epsilonRate = bitTypeCounters.epsilonRate
    val powerConsumption = bitTypeCounters.powerConsumption

    println(s"Here is our bit counters $bitTypeCounters")
    println(s"Power consumption $powerConsumption")
  }
}
