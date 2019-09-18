abstract class Nat {
  def isZero: Boolean
  def predecessor: Nat
  def successor: Nat = new Succ(this)
  def +(that: Nat): Nat
  def -(that: Nat): Nat
}

object Zero extends Nat {
  def isZero = true
  override def predecessor: Nat = throw new Error("0.predecessor")
  def +(that: Nat): Nat = that
  def -(that: Nat): Nat =
    if (that.isZero) this else throw new Error("0.minus is a negative")
  override def toString = " 0 "
}

class Succ(n: Nat) extends Nat {
  def isZero = false
  def predecessor = n
  def +(that: Nat) = new Succ(n + that)
  def -(that: Nat) = if (that.isZero) this else n - that.predecessor
  override def toString = predecessor.toString() + " +1 "
}

object idgi extends App {
  val zero = Zero
  val one = zero.successor
  println(zero)
  println(one)
  println(one.successor)
  println(one.successor - one)
  println(one - one)
  println(
    "four minus two is ==> " + (new Succ(new Succ(new Succ(one))) - new Succ(
      one
    ))
  )
}
