import reductions.ParallelParenthesesBalancing

val l =List(0,1,2,-5,4,5,6,7,8)
val threshold = 5

val len = 10
val v = Range(0,len) by 1
val r = v zip v.tail :+ len

val xs: Array[Int] = 
