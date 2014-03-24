package recfun
import common._

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
    if (c < 0 || r < 0)
      throw new IllegalArgumentException()

    if (c == 0 || c == r)
      1
    else
      pascal(c - 1, r - 1) + pascal(c, r - 1)
  }

  /**
   * Exercise 2
   */
  def balance(chars: List[Char]): Boolean = {
    def inner(chars: List[Char], counter: Int): Boolean = {
      if (chars.isEmpty) counter == 0
      else if (chars.head == '(') inner(chars.tail, counter + 1)
      else if (chars.head == ')') counter > 0 && inner(chars.tail, counter - 1)
      else inner(chars.tail, counter)
    }
    inner(chars, 0)
  }

  /**
   * Exercise 3
   */
  def countChange(money: Int, coins: List[Int]): Int = {
    def inner(money: Int, coins: List[Int]): Int = {
      if (coins.length == 0 || money < 0) 0
      else if (money == 0) 1
      else inner(money, coins.tail) + inner(money - coins.head, coins)
    }
    inner(money, coins.sorted)
  }
}
