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
    var accum = 0;
    var i = 0;
    while (i < chars.length){
      if (chars(i) == '(') accum += 1
      else if (chars(i) == ')') accum -= 1

      if (accum < 0 ) return false
      i += 1
    }
    accum == 0
  }

  /** Returns `true` iff the parentheses in the input `chars` are balanced.
   */
  def parBalance(chars: Array[Char], threshold: Int): Boolean = {

    def traverse(idx: Int, until: Int, arg1: Int, arg2: Int) : (Int, Int) = {
      if (idx >= until) (arg1, arg2)
      else if (chars(idx) == '(') traverse(idx + 1, until, arg1 + 1, arg2)
      else if (chars(idx) == ')') {
        if (arg1 > 0) traverse(idx + 1, until, arg1 - 1, arg2)
        else traverse(idx + 1, until, arg1, arg2 + 1)
      }
      else traverse(idx + 1, until, arg1, arg2)
    }


    def reduce(from: Int, until: Int): Int = {

    if (until <= from + threshold) {
        val (left, right) = traverse(from, until, 0, 0)
        left - right
      }
      else {
        val (x, y) = parallel(traverse(from, threshold, 0, 0), reduce(from + threshold, until))
        if (y > 0) return -1
        x._1 - x._2 + y

      }


//      val splitPoints = Range(from, until) by threshold
//      val tasks = splitPoints zip splitPoints.tail :+ until
//
//      val res = tasks.reduce{
//        (acc, next) => {val (left, right) =
//          parallel(traverse(acc._1, acc._2, 0, 0),traverse(next._1, next._2, 0, 0))
//
//          if (left._1 < left._2) return -1
//          else if ((left._1 + right._1) < (left._2 + right._2)) return -1
//          else {
//            val l = left._1 + right._1
//            val r = left._2 + right._2
//
//            (l -r, r - r)
//          }
//        }
//      }
//      res._1 - res._2

//      tasks.map{
//        case (start, stop) => task{
//          val (left, right) = traverse(start, stop, 0, 0)
//          left - right
//        }
//      }.map(_.join())
//        .foldLeft(0)((l, r) => {
//          if (l + r < 0) return -1
//          else l + r
//        })

    }

    reduce(0, chars.length) == 0


  }

  // For those who want more:
  // Prove that your reduction operator is associative!

}
