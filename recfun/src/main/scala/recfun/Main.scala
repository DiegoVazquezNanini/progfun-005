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
    if (c == 0) 1
    else {
      if (c > r) 0
      else pascal(c,r-1) + pascal(c-1,r-1)
    }
  }

  /**
   * Exercise 2
   */
  def balance(chars: List[Char]): Boolean = {

    def isOpen(c: Char): Boolean = {
      c == '('
    }

    def isParenthesis(c: Char): Boolean = {
      c == '(' || c == ')'
    }

    def balanceWithStack(chars: List[Char], stack: Int): Boolean = {

      //println("chars: " + chars + ", stack: " + stack)

      if (stack < 0) false
      else {
        if (chars.isEmpty) stack == 0
        else {
          if (isParenthesis(chars.head)) {
            if (isOpen(chars.head)) {
              balanceWithStack(chars.tail, stack + 1)
            }
            else {
              balanceWithStack(chars.tail, stack - 1)
            }
          }
          else balanceWithStack(chars.tail, stack)
        }
      }

    }

    balanceWithStack(chars, 0)

  }

  // (()())   true
  // (()) ()  true
  // )        false
  // (()      false
  // ())(     false

  /**
   * Exercise 3
   */
  def countChange(money: Int, coins: List[Int]): Int = {
    if (money < 0 || coins.isEmpty) 0
    else if (money == 0) 1
    else countChange(money - coins.head, coins) + countChange(money, coins.tail)
  }
}
