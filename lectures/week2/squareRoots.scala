import math.abs

def sqrt(x: Double): Double = {

  def isGoodEnough(guess: Double, x: Double): Boolean = {
    abs((guess * guess) - x) < 0.001 * x
  }

  def improve(guess: Double, x: Double): Double = {
    (x / guess + guess) / 2
  }

  def sqrtIter(guess: Double = 1, x: Double): Double = {
    if (isGoodEnough(guess, x)) guess
    else sqrtIter(improve(guess, x), x)
  }

  sqrtIter(1, x)
}


Array(1, 2, 3, 4, 5, 9, 25, 69, 64)
  .map(_.toDouble)
  .map(sqrt)
  .map(println)

println(sqrt(1e-6))
println(sqrt(1e60))
