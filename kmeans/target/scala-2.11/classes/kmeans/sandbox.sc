import java.util.concurrent._

import scala.collection.{GenMap, GenSeq}

def sum(xs: Array[Int]): Int = {
  xs.par.fold(0)(_ + _)
}

val xs = Array(1,2,3,4,5,6)

sum(xs)

def max2(xs: Array[Int]): Int = {
  xs.par.fold(Int.MinValue)((m, x) => {
    if (m > x) m else x
    //or math.max
  })
}

max2(xs)

def isVowel(char: Char): Boolean = {
  val vowels = "AEIOUY"
  vowels.contains(char)
}

val chars = Array('A','B','R', 'E', 'Y')

chars.par.aggregate(0)(
  (count, c) => if (isVowel(c)) count + 1 else count,
  _ + _
)

List(1,2,3).par

trait Iterator[T]{
  def hasNext: Boolean
  def next: T
  def foldLeft[S](z: S)(f: (S, T) => S): S = {
    var result = z
    if(hasNext) {result = f(result, next); foldLeft(result)(f)}
    result
  }
}

//trait Splitter[T] {
//  def threshold: Int
//  def split: Seq[Splitter[T]]
//  def remaining: Int
//  def fold(z:T)(f: (T, T) => T): T = {
//    if (remaining < threshold) foldLeft(z)(f)
//    else {
//      val children: Seq[ForkJoinTask[T]] = split.map(child => task{child.fold(z)(f)})
//      children.map(_.join()).foldLeft(z)(f)
//    }
//  }
//}


