package test

import org.specs2.Specification
import org.specs2.matcher.DataTables
import scala.math._
import scalaz._
import Scalaz._
import Memo._
import org.specs2.main.CommandLineArguments

class AbcNumbersSpec extends Specification with DataTables with CommandLineArguments { def is = s2"""

  2^a * 3^b * 5^c

  first values              $firstValues
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

  def tenThousand = find(arguments.commandLine.int("n").getOrElse(10000)) must_== abcNumber((5, 10, 16))

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
    foundNumbers.sorted.drop(n - 1).head
  }

  /** @return all the triplets having a sum == s */
  def tripletsOfSum = { (s: Int) =>
    (0 to s).flatMap(i => (0 to s - i).map(j => (i, j, s - i - j)))
  }

  /**
   * Math functions
   */
  val abcNumber: Triplet => BigInt =  { case (a, b, c) => BigInt(2).pow(a) * BigInt(3).pow(b) * BigInt(5).pow(c) }

  /**
   * types definitions
   */
  type Triplet = (Int, Int, Int)
}


