package test

import org.specs2.Specification
import org.specs2.matcher.DataTables
import scala.math._
import scalaz._
import Scalaz._
import Memo._
import org.specs2.main.CommandLineArguments

class AbcNumbersSpec extends Specification with DataTables with CommandLineArguments { def is = s2""" $sequential

  2^a * 3^b * 5^c

  first values              $firstValues

  10.000th value            $tenThousand
  10.000th value            $tenThousand
  10.000th value            $tenThousand
  10.000th value            $tenThousand
  10.000th value            $tenThousand
  10.000th value            $tenThousand
  10.000th value            $tenThousand
  10.000th value            $tenThousand
  10.000th value            $tenThousand
  10.000th value            $tenThousand
  10.000th value            $tenThousand
  10.000th value            $tenThousand
  10.000th value            $tenThousand
  10.000th value            $tenThousand
  10.000th value            $tenThousand
  10.000th value            $tenThousand

"""

  /**
   * TESTS
   */
  def firstValues = {
    "n"   | "a"   | "b"  | "c"   | "number" |>
    1     ! 0     ! 0    ! 0     ! 1        |
    2     ! 1     ! 0    ! 0     ! 2        |
    3     ! 0     ! 1    ! 0     ! 3        |
    4     ! 2     ! 0    ! 0     ! 4        |
    5     ! 0     ! 0    ! 1     ! 5        |
    6     ! 1     ! 1    ! 0     ! 6        |
    7     ! 3     ! 0    ! 0     ! 8        |
    8     ! 0     ! 2    ! 0     ! 9        |
    9     ! 1     ! 0    ! 1     ! 10       | { (n, a, b, c, number) =>
      find(n) must_== number
    }
  }

  def tenThousand = findFunctional(arguments.commandLine.int("n").getOrElse(10000)) must_== abcNumber((5, 10, 16))

  /**
   * find the nth number of the form 2^a* 3^b * 5^c
   */
  def find(n: Int): BigInt = {
    if (n <= 0) throw new IllegalArgumentException(s"$n must be positive")

    var maxNumber    = BigInt(0)
    var tripletSum   = 0
    val foundNumbers = new scala.collection.mutable.ListBuffer[BigInt]

    /**
     * find at least n numbers by evaluating all triplets of size 'tripletSum = a + b + c'
     * increasing this value progressively
     */
    while (foundNumbers.size < n) {
      tripletsOfSum(tripletSum).foreach { t =>
        val number = abcNumber(t)
        foundNumbers.append(number)
        if (number > maxNumber) maxNumber = number // update the maximum of the abc numbers found so far
      }
      tripletSum += 1
    }

    /**
     * Some numbers might be missing, go on increasing the value of tripletSum
     * until 2^tripletSum > maxNumber. Then we know that any new number will be superior to the n already found
     */
    while (BigDecimal.valueOf(pow(2, tripletSum)) <= BigDecimal.valueOf(maxNumber.toDouble)) {
      tripletsOfSum(tripletSum).foreach { t =>
        foundNumbers.append(abcNumber(t))
      }
      tripletSum += 1
    }

    // return the first n numbers
    foundNumbers.sorted.apply(n - 1)
  }

  /**
   * find the nth number of the form 2^a* 3^b * 5^c
   * using no variables
   */
  def findFunctional(n: Int): BigInt = {
    if (n <= 0) throw new IllegalArgumentException(s"$n must be positive")

    // generate number of the form 2^a* 3^b * 5^c
    // keeping track of the maximum and of the current sum of coefficients
    lazy val generateNumbers =
      Stream.iterate((BigInt(0), 0, Vector[BigInt]())) { case (max, s, nbs) =>
        val (newMax, newNumbers) =
          tripletsOfSum(s).foldLeft((max, nbs)) { case ((max1, nbs1), triplet) =>
            val number = abcNumber(triplet)
            (if (number > max1) number else max1, nbs1 :+ number)
          }

        (newMax, s + 1, newNumbers)
      }

    // take the first list of numbers having at least n numbers
    val (_, n2) = generateNumbers.span { case (_, _, nbs) => nbs.size < n }
    val (maximum, sum, numbers) = n2.head

    // we need to find more numbers but we stop adding them when the sum of the coefficients 
    // will only create numbers bigger than the maximum
    // maxSum > log(maximum)/log(2)
    val maxSum = log2(maximum) + 1

    val moreNumbers = (sum to maxSum).foldLeft(numbers) { case (seq, s) =>
      tripletsOfSum(s).foldLeft(seq) { (seq1, t) => seq1 :+ abcNumber(t) }
    }

    // take the nth number
    moreNumbers.sorted.apply(n - 1)
  }

  /** @return all the triplets having a sum == s */
  def tripletsOfSum = { (s: Int) =>
    (0 to s).flatMap(i => (0 to s - i).map(j => (i, j, s - i - j)))
  }

  /**
   * Math functions
   */
  def log2(n: BigInt): Int = math.floor(math.log(n.toDouble) / math.log(2)).toInt
  val abcNumber: Triplet => BigInt =  { case (a, b, c) => BigInt(2).pow(a) * BigInt(3).pow(b) * BigInt(5).pow(c) }

  /**
   * types definitions
   */
  type Triplet = (Int, Int, Int)
}


