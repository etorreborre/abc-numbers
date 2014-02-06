package test

import org.specs2.Specification
import org.specs2.matcher.DataTables
import scala.math._
import scalaz._
import Scalaz._
import Memo._
import org.specs2.main.CommandLineArguments
import scala.annotation.tailrec

class AbcNumbersSpec extends Specification with DataTables with CommandLineArguments { def is = s2"""

  2^a * 3^b * 5^c

  find all the triplets which sum is == n $triplets
  find the first values                   $firstValues
  10.000th value                          $tenThousand

  find candidates $candidates

  newThing $newThing
"""

  def newThing = {
    findNext(1000).pp
    ok

  }

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

  def candidates = {
    val n = arguments.commandLine.int("n").getOrElse(10)
    findAbcNumber(n).pp
    find(n).pp("is correct")
    ok
  }

  /** find possible combinations */
  type Triplet = (Int, Int, Int)
  type Pair    = (Int, Int)

  def tripletsEqualTo = mutableHashMapMemo((n: Int) =>
    (0 to n).flatMap(i => pairsEqualTo(n - i).flatMap(pair => permutations(i, pair))).toSet.toSeq)

  def pairsEqualTo = mutableHashMapMemo((n: Int) =>
    (0 to n).map(i => (i, n - i)))

  def permutations(n: Int, pair: Pair): Seq[Triplet] =
    Seq(n, pair._1, pair._2).permutations.map { case a :: b :: c :: _ => (a, b, c) }.toSeq

  /*

  2^a * 3^b * 5^c + 2^a1 * 3^b1 * 5^c1

  log(fn) = alog2 + blog3 + clog5

  log2 >= log(fn)-log(prev) > 0
  alog2 + blog3 + clog5 > log(prev)

  a >= 0
  a <= abs(bprev + cprev - aprev)

  blog3 + clog5 > C

  c = round(C - blog3)/ log5

  log(n)   = a * log(2) + b * log(3) + c * log(5)
  log(n+1) = xlog2 + ylog3 + zlog5

  log(n+1/n) = (x-a)log2 + (y-b)log3 + (z-c)log5

  log(2*n/2) = log2 + log(n/2)
  log(n/2) = (a - 1) * log(2) + blog3 + clog5

  log(n+1) min && log(n+1) > log(n)

  a == 0, b > roundup(min - clog5)/log2
  a = roundup((min - blog3 - clog5) / log2)


   */
  def findNext(n: Int): Seq[BigInt] = {
    def isDiv2(n: BigInt) = Seq('0', '2', '4', '6', '8').exists(_ == n.toString.last)
    def isDiv3(n: BigInt): Boolean = {
      val sum = n.toString.split("").filter(_.nonEmpty).map(_.toInt).sum
      sum == 3 || (n.toString.size >=2 && isDiv3(sum))
    }
    def isDiv5(n: BigInt) = Seq('0', '5').exists(_ == n.toString.last)

    if (n == 1) Seq(1)
    else {
      val previous = findNext(n - 1)
      s"previous is $previous"
      var trying = previous.last + BigInt(1)
      var found = false
      while (!found) {
        s"trying $trying".pp
        found =
          (isDiv2(trying) && previous.contains(trying / 2)) ||
            (isDiv3(trying) && previous.contains(trying / 3)) ||
            (isDiv5(trying) && previous.contains(trying / 5))
        if (!found) trying = trying + BigInt(1)
      }
      previous :+ trying
    }
  }

  /**
   * find number at step n:
   *
   *  - find the number at step n - 1
   *  2^x * 3^y * 5^z > 1
   *  abs(x) <= a, abs(y) <= b, abs(z) <= c
   *  abs(x + y + z) < abs(minValue)
   */
  def findAbcNumber(n: Int): (Triplet, BigInt) = {
    def range(x: Int, y: Int) = (- x to y).toList
    def findMin(a: Int, b: Int, c: Int, maxSum: Int): Triplet = {
      (range(a, a + maxSum) |@| range(b, b + maxSum) |@| range(c, c + maxSum))((_,_,_)).
        filter { case (x, y, z) => abs(x + y + z) <= maxSum }.
        map    { case t => (t, abcDouble(t)) }.
        filter { _._2 > 1.0 }.toList.minBy(_._2)._1
    }

    def abcNumbers: Stream[(Triplet, BigInt)] = {
      var n = 0
      Stream.iterate(((0, 0, 0), BigInt(1))) { case ((a, b, c), number) =>
        val maximumValueForAbc = log2(number) + 1
        val (x, y, z) = findMin(a, b, c, maximumValueForAbc)
        val triplet = (a + x, b + y, c + z)
        val (tw, th, fi) = triplet
        n = n + 1
//        if (tw != 0 && th != 0 && fi != 0) "divisible par 30".pp
//        if (tw == 0 && th != 0 && fi != 0) "divisible par 15".pp
//        if (tw == 0 && th == 0 && fi != 0) "divisible par 5".pp
//        if (tw != 0 && th != 0 && fi == 0) "divisible par 6".pp
//        if (tw == 0 && th != 0 && fi == 0) "divisible par 3".pp
//        if (tw != 0 && th == 0 && fi != 0) "divisible par 10".pp
//        if (tw != 0 && th == 0 && fi == 0) "divisible par 2".pp

        val abc = abcNumber(triplet)
        //(min, max).pp("min, max for abc")
        //        (log2(abc), log3(abc), log5(abc)).pp(s"logs for $abc")
        s"interim result for ${n+1} ${(triplet, abc)} ".pp
        (triplet, abc)
      }
    }
    if (n <= 0) throw new IllegalArgumentException(s"$n must be positive")
    else abcNumbers(n - 1).pp(s"result for $n")
  }


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


  def valuesWhereTripletBetween: (Int, Int, Double) => Seq[(Triplet, Double)] = { (min: Int, max: Int, maxLog: Double) =>
    if (max < min) Seq()
    else           (valuesWhereTripletEqualTo(max).filterNot(_._2 < maxLog).filterNot(_._2 > maxLog+log235) ++ valuesWhereTripletBetween(min, max - 1, maxLog)).sortBy(_._2)
  }

  def valuesWhereTripletEqualTo = mutableHashMapMemo((n: Int) =>
    tripletsEqualTo(n).map(triplet => (triplet, abcLog(triplet))))

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


