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
    def doPascal(x: Int, y: Int): Int = if (x == 0 || y == 0 || x == y) 1 else doPascal(x - 1, y - 1) + doPascal(x, y - 1)
    doPascal(c, r)
  }

  /**
    * Exercise 2
    */
  def balance(chars: List[Char]): Boolean = {
    def doBalance(c: List[Char], q: Int): Boolean = c match {
      case Nil => q == 0
      case h :: t if h == '(' => doBalance(t, q + 1)
      case h :: t if h == ')' => if (q <= 0) false else doBalance(t, q - 1)
      case h :: t => doBalance(t, q)
    }
    doBalance(chars, 0)
  }

  /**
    * Exercise 3
    */
  def countChange(money: Int, coins: List[Int]): Int = {
    if (money == 0) 1
    else if (coins.isEmpty || money < 0) 0
    else countChange(money - coins.head, coins) + countChange(money, coins.tail)
  }
}
