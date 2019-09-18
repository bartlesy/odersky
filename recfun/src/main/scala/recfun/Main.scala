package recfun

object Main {
  def main(args: Array[String]) {
    println("Pascal's Triangle")
    for (row <- 0 to 10) {
      for (col <- 0 to row)
        print(pascal(col, row) + " ")
      println()
    }
  }

  /**
    * Exercise 1
    */
  def pascal(c: Int, r: Int): Int = {
    if (r <= 0 || c <= 0 || c == r) 1
    else pascal(c - 1, r - 1) + pascal(c, r - 1)
  }

  /**
    * Exercise 2
    */
  def balance(chars: List[Char]): Boolean = {
    def str_to_parens(chars: List[Char]): List[Char] = {
      chars.filter(c => Set('(', ')').contains(c))
    }

    def _balance(stack: List[Char], parens: List[Char]): Boolean = {
      if (parens.isEmpty) {
        stack.isEmpty
      } else if (parens.head == '(') {
        _balance(stack ++ List(parens.head), parens.tail)
      } else {
        if (parens.head == ')' && stack.isEmpty) {
          false
        } else _balance(stack.tail, parens.tail)
      }
    }

    val parens = str_to_parens(chars)
    _balance(List(), parens)
  }

  /**
    * Exercise 3
    */
  def countChange(money: Int, coins: List[Int]): Int = {
    if (money == 0) {
      1
    } else {
      if (coins.isEmpty) {
        0
      } else {
        if (coins.last > money) {
          countChange(money, coins.dropRight(1))
        } else {
          countChange(money - coins.last, coins) + countChange(
            money,
            coins.dropRight(1)
          )
        }
      }
    }
  }
}
