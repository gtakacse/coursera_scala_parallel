val x = 1
val y = 2

val r = 1

var i = x-r


while (i <= x + r){
  var j = y-r
  while (j <= y + r){
    print(s"($i,$j)\n")
    j += 1
  }
  i+=1
}

val h = 21
val n = 5
val v = Range(0, h) by Math.max(h/n, 1)
v zip v.tail :+ h


