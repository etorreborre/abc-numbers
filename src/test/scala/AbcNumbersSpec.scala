package test

import org.specs2.Specification
import org.specs2.matcher.DataTables
import scala.math._
import scalaz._
import Scalaz._
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
      find(n) must_== ((a, b, c), number)
    }
  }

  def tenThousand = find(arguments.commandLine.int("n").getOrElse(10000)) must_== ((5, 10, 16), abcNumber((5, 10, 16)))

  /**
   * types definitions
   */
  type Triplet = (Int, Int, Int)

  /**
   * Main algorithm
   *
   * find the nth number:
   *
   *  - find the number nth - 1 (previousNumber)
   *
   *  - we search for the coefficients (a, b, c) of number, by searching in a subset of all possible coefficients
   *
   *  knowing that:
   *     - nth number = 2^a*3^b*5^c
   *     - nth number > previousNumber
   *
   *  We can bound the search by adding those inequalities
   *     - a + b + c <= log2(previousNumber) + 1 (in case the new number is a power of 2, the sum of a + b + c will be maximal)
   *     - a + b + c >= log5(previousNumber) + 1 (in case the new number is a power of 5, the sum of a + b + c will be minimal)
   *     - b <= log3(previousNumber) + 1 if that helps
   *
   *  Then instead of searching the coefficients so that:
   *    2^a*3^b*5^c is minimum and
   *    2^a*3^b*5^c > previousNumber
   *
   *  We look for the coefficients that minimize:
   *    a*log2 + b*log3 + clog5 and
   *    a*log2 + b*log3 + clog5 > log(previousNumber) - because log is monotonic
   *
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
        val previousLog        = log(previousNumber.toDouble + 0.5)

        val abc = findBestAbc(minimumValueForAbc, maximumValueForB, maximumValueForAbc, previousLog)
        solution = (abc, abcNumber(abc))
        //if (i % 100 == 0) solution.pp(s"current result for $i")
        i = i + 1
      }
      solution
    }
  }

  /**
   * branch and bound algorithm to find (a, b, c) to miminize the a*log2 + b*log3 + c*log5 function ('abcLog')
   *
   * so that abcLog(a, b, c) > minLog and minSum <= a + b + c <= maxSum (and b <= maxB)
   *
   *  1. we start with a list of "Nodes" representing possible sets of values for a, b and c
   *
   *  2. we evaluate the minimum and maximum possible values for the abcLog function over each node
   *
   *  3. we update the best found minimum for the abcLog function
   *
   *  4. we remove all the nodes which we know will not be better or not satisfy the > minLog constraint (pruning)
   *
   *  5. we split the remaining ranges into smaller ones (the "branch" method) and we iterate until one solution is left
   *
   */
  def findBestAbc(minSum: Int, maxB: Int, maxSum: Int, minLog: Double): Triplet = {

    /**
     * branching method from a given "node" in the search tree
     *
     *  - if any range of the Node can be split (hence the checks for size == 1), we split it
     *  - we always split the biggest range
     *  - the split method (implemented on Node) "cuts" a given range (on 'a' values, 'b' values or 'c' values) into
     *    2 equal ranges: for example [0, 1, 2, 3] is splitted into [0, 1] and [2, 3]
     *
     * we remove from the branching any node having ranges which offer no possible combinations so that the
     * sum of coefficients is between minSum and maxSum
     */
    def branch(node: Node): Seq[Node] = {
      val branched =
        if (node.aRange.size == 1) {
          if (node.bRange.size == 1) {
            if (node.cRange.size == 1) Seq(node)
            else node.splitc
          } else
            if (node.bRange.size >= node.cRange.size) node.splitb
            else                                      node.splitc
        } else
          if (node.aRange.size >= node.bRange.size && node.aRange.size >= node.cRange.size) node.splita
          else if (node.bRange.size >= node.aRange.size && node.bRange.size >= node.cRange.size) node.splitb
          else node.splitc

      branched.filter(n => n.isSumBetween(minSum, maxSum) && n.bRange.head <= maxB)
    }

    /**
     * initialize values
     */
    // the first node, root of the searc true
    val initialRange = 0 to maxSum
    val start = Node(initialRange, initialRange, initialRange)

    // at first the bestLog is not found (set to infinite)
    var bestLog  = Double.MaxValue
    // we branch once
    var nodes = Seq(start).flatMap(branch)

    // we search until there's only one possibility left
    while (nodes.size > 1) {
      // compute the bounds on each node
      val bounds = nodes.map(n => (n, n.bound))

      // compute the minimum possible value on each node so that this value is above the required minimum
      // this is the new best objective
      val minLogs = bounds.map { case (n, (min, max)) => min }.filter(_ >= minLog)
      if (!minLogs.isEmpty) bestLog = minLogs.min

      // remove all nodes having a minimum bound already superior to the best solution
      // or a maximum bound which is below the required minimum
      val pruned = bounds.filter { case (n, (min, max)) => min <= bestLog && max >= minLog }.map(_._1)
      // branch again on each node (flatMap applies a function A => Seq[A] to each element and 'flatten' the results into one
      // sequence)
      nodes = pruned.flatMap(branch)
    }

    // return the remaining value
    nodes.head.triplet
  }

  /**
   * A node encapsulates sets of values for the coefficients (a, b, c):
   *   - a "range" for a
   *   - a "range" for b
   *   - a "range" for c
   *
   * and a few utility functions
   */
  case class Node(aRange: Seq[Int], bRange: Seq[Int], cRange: Seq[Int]) {
    def splita = {
      val (s1, s2) = splitRange(aRange)
      Seq(copy(aRange = s1), copy(aRange = s2))
    }
    def splitb = {
      val (s1, s2) = splitRange(bRange)
      Seq(copy(bRange = s1), copy(bRange = s2))
    }
    def splitc = {
      val (s1, s2) = splitRange(cRange)
      Seq(copy(cRange = s1), copy(cRange = s2))
    }

    /** split a range into 2 */
    def splitRange(range: Seq[Int]): (Seq[Int], Seq[Int]) =
      (range.take(range.size / 2), range.drop(range.size / 2))

    def minAbc = (aRange.head, bRange.head, cRange.head)

    def maxAbc = (aRange.last, bRange.last, cRange.last)

    /** @return the min and max possible values of the abcLog function for this Node */
    def bound = (abcLog(minAbc), abcLog(maxAbc))

    /**
     * @return true if there is a possible combination of coefficients
     *         where the sum of coefficients is between min and max
     */
    def isSumBetween(min: Int, max: Int) =
        // if the sum of the minimum coefficients ('head' of each range) is > max or
        // if the sum of the maximum coefficients ('last' of each range) is < min
        // then we know that the Node needs to be excluded
      !(Seq(aRange.head, bRange.head, cRange.head).sum > max ||
        Seq(aRange.last, bRange.last, cRange.last).sum < min)

    /** return the 'minimum' triplet */
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


