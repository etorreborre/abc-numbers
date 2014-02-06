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

  10.000th value                          $tenThousand

"""

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

  /**
   * find number at step n:
   *
   *  - find the number at step n - 1
   *  - the next number has a maximum value x for one of a, b, c == log2(previousNumber) + 1
   *                        and minimum value x for one of a, b, c == log5(previousNumber)
   *  - calculate values among all the possible triplets and sort them
   *  - take the minimum value that is > to the previous number
   */
  def find(n: Int): (Triplet, BigInt) = {
    if (n <= 0) throw new IllegalArgumentException(s"$n must be positive")
    else if (n == 1) ((0, 0, 0), BigInt(1))
    else if (n == 2) ((1, 0, 0), BigInt(2))
    else {
      var solution = ((1, 0, 0), BigInt(2))
      var i = 2

      while (i != n) {
        val (_, previousNumber) = solution

        val maximumValueForAbc = log2(previousNumber) + 1
        val minimumValueForAbc = log5(previousNumber) + 1
        val maximumValueForB   = log3(previousNumber) + 1
        val previousLog = log(previousNumber.toDouble + 0.5)

        val abc = findBestAbc(minimumValueForAbc, maximumValueForB, maximumValueForAbc, previousLog)
        solution = (abc, abcNumber(abc))
        if (i % 100 == 0) solution.pp(s"current result for $i")
        i = i + 1
      }
      solution
    }
  }

  def findBestAbc(minSum: Int, maxB: Int, maxSum: Int, minLog: Double): Triplet = {

    def branch(node: Node): Seq[Node] = {
      val branched =
        if (node.aRange.size == 1) {
          if (node.bRange.size == 1) {
            if (node.cRange.size == 1) Seq(node)
            else Seq(node.takeHalfc, node.dropHalfc)
          } else
            if (node.bRange.size >= node.cRange.size) Seq(node.takeHalfb, node.dropHalfb)
            else                                      Seq(node.takeHalfc, node.dropHalfc)
        } else
          if (node.aRange.size >= node.bRange.size && node.aRange.size >= node.cRange.size) Seq(node.takeHalfa, node.dropHalfa)
          else if (node.bRange.size >= node.aRange.size && node.bRange.size >= node.cRange.size) Seq(node.takeHalfb, node.dropHalfb)
          else Seq(node.takeHalfc, node.dropHalfc)

      branched.filter(n => n.isSumBetween(minSum, maxSum) && n.bRange.head <= maxB)
    }

    val initialRange = 0 to maxSum
    val start = Node(initialRange, initialRange, initialRange)

    var bestLog  = Double.MaxValue
    var nodes = Seq(start).flatMap(branch)

    while (nodes.size > 1) {
      val bounds = nodes.map(n => (n, n.bound))
      val minLogs = bounds.map { case (n, (min, max)) => min }.filter(_ >= minLog)
      if (!minLogs.isEmpty) bestLog = minLogs.min

      val pruned = bounds.filter { case (n, (min, max)) => min <= bestLog && max >= minLog }.map(_._1)
      nodes = pruned.flatMap(branch)
    }

    nodes.head.triplet
  }


  case class Node(aRange: Range, bRange: Range, cRange: Range) {
    def takeHalfa = copy(aRange = aRange.take(aRange.size / 2))
    def takeHalfb = copy(bRange = bRange.take(bRange.size / 2))
    def takeHalfc = copy(cRange = cRange.take(cRange.size / 2))

    def dropHalfa = copy(aRange = aRange.drop(aRange.size / 2))
    def dropHalfb = copy(bRange = bRange.drop(bRange.size / 2))
    def dropHalfc = copy(cRange = cRange.drop(cRange.size / 2))

    def minAbc = (aRange.head, bRange.head, cRange.head)
    def maxAbc = (aRange.last, bRange.last, cRange.last)

    def bound = (abcLog(minAbc), abcLog(maxAbc))

    def isSumBetween(min: Int, max: Int) =
      !(Seq(aRange.head, bRange.head, cRange.head).sum > max ||
        Seq(aRange.last, bRange.last, cRange.last).sum < min)

    def triplet = minAbc
  }


  /**
   * math functions
   */
  def log2(n: BigInt): Int = math.floor(math.log(n.toDouble) / math.log(2)).toInt
  def log3(n: BigInt): Int = math.floor(math.log(n.toDouble) / math.log(3)).toInt
  def log5(n: BigInt): Int = math.floor(math.log(n.toDouble) / math.log(5)).toInt

  val (log2, log3, log5) = (log(2), log(3), log(5))
  val log235 = log2+log3+log5
  val abcLog: Triplet => Double =  { case (a, b, c) => a*log2 + b*log3 + c*log5 }

  val abcNumber: Triplet => BigInt =  { case (a, b, c) => BigInt(2).pow(a) * BigInt(3).pow(b) * BigInt(5).pow(c) }
  val abcDouble: Triplet => BigDecimal =  { case (a, b, c) => BigDecimal(2).pow(a) * BigDecimal(3).pow(b) * BigDecimal(5).pow(c) }

}


