import math.abs

object exercise extends App {
  val tolerance = 0.0001

  def isCloseEnough(x: Double, y: Double) = {
    abs((x - y) / x) / x < tolerance
  }

  def fixedPoint(f: Double => Double)(firstGuess: Double): Double = {
    def iterate(guess: Double): Double = {
      val next = f(guess)
      if (isCloseEnough(guess, next)) next
      else iterate(next)
    }
    iterate(firstGuess)
  }

  println(fixedPoint(x => 1 + x / 2)(1))

  def averageDamp(f: Double => Double)(x: Double) = (x + f(x)) / 2

  def sqrt(x:Double): Double = {
    fixedPoint(averageDamp(y => x / y))(1)
  }

  println(sqrt(9))

  def dblFunc(f: Double => Double)(x: Double) = f(x) * 2

  def halfer(f: Double => Double)(x: Double) = f(x) / 2

  println(dblFunc(halfer(identity))(10))
}

