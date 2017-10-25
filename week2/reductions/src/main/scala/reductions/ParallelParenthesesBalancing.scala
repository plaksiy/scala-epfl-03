package reductions

import scala.annotation._
import org.scalameter._
import common._

object ParallelParenthesesBalancingRunner {

  @volatile var seqResult = false

  @volatile var parResult = false

  val standardConfig = config(
    Key.exec.minWarmupRuns -> 40,
    Key.exec.maxWarmupRuns -> 80,
    Key.exec.benchRuns -> 120,
    Key.verbose -> true
  ) withWarmer(new Warmer.Default)

  def main(args: Array[String]): Unit = {
    val length = 100000000
    val chars = new Array[Char](length)
    val threshold = 10000
    val seqtime = standardConfig measure {
      seqResult = ParallelParenthesesBalancing.balance(chars)
    }
    println(s"sequential result = $seqResult")
    println(s"sequential balancing time: $seqtime ms")

    val fjtime = standardConfig measure {
      parResult = ParallelParenthesesBalancing.parBalance(chars, threshold)
    }
    println(s"parallel result = $parResult")
    println(s"parallel balancing time: $fjtime ms")
    println(s"speedup: ${seqtime / fjtime}")
  }
}

object ParallelParenthesesBalancing {

  /** Returns `true` iff the parentheses in the input `chars` are balanced.
   */
  def balance(chars: Array[Char]): Boolean = {
    @scala.annotation.tailrec
    def loop(idx: Int, last: Int, count: Int): Boolean = {
      if (idx == last)
        count == 0
      else if (')' == chars(idx))
        if (count == 0) false
        else loop(idx + 1, last, count - 1)
      else if ('(' == chars(idx))
        loop(idx + 1, last, count + 1)
      else
        loop(idx + 1, last, count)
    }

//  TODO This implementation (using equals) are 5 times slowly !!!
    @scala.annotation.tailrec
    def loop1(idx: Int, last: Int, count: Int): Boolean = {
      if (idx == last)
        count == 0
      else if (')' equals chars(idx))
        if (count == 0) false
        else loop1(idx + 1, last, count - 1)
      else if ('(' equals chars(idx))
        loop1(idx + 1, last, count + 1)
      else
        loop1(idx + 1, last, count)
    }

//  TODO This implementation (using match) are 4 times slowly !!!
    @scala.annotation.tailrec
    def loop2(idx: Int, last: Int, count: Int): Boolean = {
      if (idx == last)
        count == 0
      else chars(idx) match {
        case ')' =>
          if (count == 0) false
          else loop2(idx + 1, last, count - 1)
        case '(' => loop2(idx + 1, last, count + 1)
        case _ => loop2(idx + 1, last, count)
      }
    }

    if (chars.isEmpty) true
    else loop(0, chars.length, 0)
  }

  /** Returns `true` iff the parentheses in the input `chars` are balanced.
   */
  def parBalance(chars: Array[Char], threshold: Int): Boolean = {

    @scala.annotation.tailrec
    def traverse(idx: Int, until: Int, arg1: Int, arg2: Int): (Int, Int) = {
      if (idx == until)
        (arg1, arg2)
      else if (')' == chars(idx))
        if (arg2 <= 0) traverse(idx + 1, until, arg1 + 1, arg2 - 1)
        else traverse(idx + 1, until, arg1, arg2 - 1)
      else if ('(' == chars(idx))
        traverse(idx + 1, until, arg1, arg2 + 1)
      else
        traverse(idx + 1, until, arg1, arg2)
    }

    def reduce(from: Int, until: Int): (Int, Int) = {
      if (until - from <= threshold)
        traverse(from, until, 0, 0)
      else {
        val mid = (until + from) / 2
        val ((unbalancedCloseL, countL), (unbalancedCloseR, countR)) =
          parallel(reduce(from, mid), reduce(mid, until))

        (balanceClose(unbalancedCloseL, unbalancedCloseR, countL), countL + countR)
      }
    }

    def balanceClose(unbalancedCloseL: Int, unbalancedCloseR: Int, countL: Int): Int = {
      unbalancedCloseL + (
        if (unbalancedCloseR == 0 || unbalancedCloseR <= countL) 0
        else if (countL == 0) unbalancedCloseR
        else countL - unbalancedCloseR
        )
    }

    reduce(0, chars.length) == (0, 0)
  }

  // For those who want more:
  // Prove that your reduction operator is associative!

}
