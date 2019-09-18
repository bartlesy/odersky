object CurryExercise extends App {

  def product(f: Int => Int)(a: Int, b: Int): Int = {
    if (a > b) 1
    else f(a) * product(f)(a + 1, b)
  }

  println(product(identity)(3, 5))
  println(product(x => x * x)(3, 4))

  def factorial(n: Int) = product(identity)(1, n)
  (0 to 5).map(factorial).map(println)

  def genAccumulator(accum_f: (Int, Int) => Int, base: Int, f: Int => Int)(
      a: Int,
      b: Int
  ): Int = {
    if (a > b) base
    else accum_f(f(a), genAccumulator(accum_f, base, f)(a + 1, b))
  }
  println(genAccumulator((a, b) => a * b, 1, identity)(3, 5))
  println(genAccumulator((a, b) => a * b, 1, x => x * x)(3, 4))

  def mapReduce(f: Int => Int, combine: (Int, Int) => Int, zero: Int)(
      a: Int,
      b: Int
  ): Int = {
    if (a > b) zero
    else combine(f(a), mapReduce(f, combine, zero)(a + 1, b))
  }

  println(mapReduce(identity, (a, b) => a * b, 1)(3, 5))
  println(mapReduce(x => x * x, (a, b) => a * b, 1)(3, 4))
}
