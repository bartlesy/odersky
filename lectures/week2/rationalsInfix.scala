object rationals extends App {
  val x = new Rational(1, 2)
  println(x)
  println(x.numer)
  println(x.denom)
  println(x. + (new Rational(3, 4)))
  println(x. - (new Rational(3, 4)))
  println(new Rational(2))
}

class Rational(x: Int, y: Int) {
  require(y != 0, "denominator must be non-zero")

  def this(x: Int) = this(x, 1)

  private def gcd(a: Int, b: Int): Int = if (b == 0) a else gcd(b, a % b)
  private val g = gcd(x, y)

  val numer = x / g
  val denom = y / g

  def + (that: Rational) =
    new Rational(
      numer * that.denom + that.numer * denom,
      denom * that.denom
    )

  def neg: Rational = new Rational(-numer, denom)

  def unary_- : Rational = new Rational(-numer, denom)

  def - (that: Rational) = this + (-that)

  def < (that: Rational) = numer * that.denom < that.numer * denom

  def max(that: Rational) = if (this < (that)) that else this

  override def toString = numer + "/" + denom
}

