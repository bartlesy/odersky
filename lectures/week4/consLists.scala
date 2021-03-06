object weed extends App {

  type T = AnyVal

  trait List[T] {
    def isEmpty: Boolean
    def head: T
    def tail: List[T]
  }

  class Cons[T](val head: T, val tail: List[T]) extends List[T] {
    def isEmpty: Boolean = false

    override def toString: String = this.head + "," + tail.toString
  }

  class Nil[T] extends List[T] {
    def isEmpty: Boolean = true
    def head: Nothing = throw new Error("Nil.head")
    def tail: Nothing = throw new Error("Nil.tail")
    override def toString: String = ""
  }

  def singleton[T](elem: T) = new Cons[T](elem, new Nil[T])

  def nth(list: List[T], n: Int): T = {
    if (list.isEmpty) throw new Error("Index out of bounds")
    else if (n == 0) list.head
    else nth(list.tail, n - 1)
  }

  val testList = new Cons(1, new Cons(2, new Cons(3, new Nil[T])))
  println(singleton(1))
  println(singleton(true))
  println(nth(singleton(1), 0))
  println(nth(testList, 2))
  println(nth(testList, 1))
  // println(nth(testList, 3))

  object List {
    def apply[T](x1: T, x2: T): List[T] = new Cons(x1, new Cons(x2, new Nil[T]))
    def apply[T](x1: T): List[T] = singleton(x1)
    def apply[T](x1: T, x2: T, x3: T): List[T] =
      new Cons(x1, new Cons(x2, new Cons(x3, new Nil[T])))
    def apply[T](): List[T] = new Nil[T]
  }
  println(List(1, 2))
  println(List(1, 2, 420))
}
