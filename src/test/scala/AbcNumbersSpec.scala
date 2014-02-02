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

  find all the triplets which sum is == n $triplets
  find the first values                   $firstValues
  10.000th value                          $tenThousand

"""

  def triplets = {
    tripletsEqualTo(3).sorted must_== Seq(
      (1, 1, 1),
      (2, 0, 1), (2, 1, 0), (0, 2, 1), (1, 2, 0), (0, 1, 2), (1, 0, 2),
      (3, 0, 0), (0, 3, 0), (0, 0, 3)
    ).sorted
  }

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
      find(n) must_== ((a, b, c), number)
    }
  }

  def tenThousand = find(arguments.commandLine.int("n").getOrElse(10000)) must_== ((5, 10, 16), abcNumber((5, 10, 16)))

  /** find possible combinations */
  type Triplet = (Int, Int, Int)
  type Pair    = (Int, Int)

  def tripletsEqualTo = mutableHashMapMemo((n: Int) =>
    (0 to n).flatMap(i => pairsEqualTo(n - i).flatMap(pair => permutations(i, pair))).toSet.toSeq)

  def pairsEqualTo = mutableHashMapMemo((n: Int) =>
    (0 to n).map(i => (i, n - i)))

  def permutations(n: Int, pair: Pair): Seq[Triplet] =
    Seq(n, pair._1, pair._2).permutations.map { case a :: b :: c :: _ => (a, b, c) }.toSeq

  /**
   * find number at step n:
   *
   *  - find the number at step n - 1
   *  - the next number has a maximum value x for one of a, b, c == log2(previousNumber) + 1
   *                        and minimum value x for one of a, b, c == log5(previousNumber)
   *  - calculate values among all the possible triplets and sort them
   *  - take the minimum value that is > to the previous number
   *
   */
  def find(n: Int): (Triplet, BigInt) = {
    if (n <= 0) throw new IllegalArgumentException(s"$n must be positive")
    else if (n == 1) ((0, 0, 0), 1)
    else {
      val (_, previousNumber) = find(n - 1)
      val maximumValueForAbc = log2(previousNumber) + 1
      val minimumValueForAbc = log5(previousNumber)

      val allValues = valuesWhereTripletBetween(minimumValueForAbc, maximumValueForAbc)
      allValues.dropWhile(_._2 <= previousNumber).head
    }
  }

  def valuesWhereTripletBetween: (Int, Int) => Seq[(Triplet, BigInt)] = { (min: Int, max: Int) =>
    if (max < min) Seq()
    else           (valuesWhereTripletEqualTo(max) ++ valuesWhereTripletBetween(min, max - 1)).sortBy(_._2)
  }

  def valuesWhereTripletEqualTo = mutableHashMapMemo((n: Int) =>
    tripletsEqualTo(n).map(triplet => (triplet, abcNumber(triplet))))

  /**
   * math functions
   */
  def log2(n: BigInt): Int = math.round(math.log(n.toDouble) / math.log(2)).toInt
  def log5(n: BigInt): Int = math.round(math.log(n.toDouble) / math.log(5)).toInt
  
  val abcNumber: Triplet => BigInt =  { case (a, b, c) => BigInt(2).pow(a) * BigInt(3).pow(b) * BigInt(5).pow(c) }


}
