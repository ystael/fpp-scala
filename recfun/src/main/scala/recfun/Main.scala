package recfun
import common._
import scala.annotation.tailrec

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
   *
   * Compute the binomial coefficient C(r, c) using a dumb quadratic recursion
   * via the identity C(r, c) = C(r - 1, c - 1) + C(r - 1, c).
   * @param c Column of Pascal's triangle (0 <= c <= r gives nonzero results)
   * @param r Row of Pascal's triangle
   * @return C(r, c)
   */
  def pascal(c: Int, r: Int): Int = {
    if (r < 0 || c < 0 || c > r)
      0
    else if (c == 0 || c == r)
      1
    else
      pascal(c - 1, r - 1) + pascal(c, r - 1)
  }

  /**
   * Exercise 2
   *
   * Determine whether a string (expressed as a list of characters) has balanced parentheses:
   * that is, every initial segment has at least as many ( as ), and the string as a whole has
   * the same number.
   * @param chars List of chars which is the string to test
   * @return True if the string is balanced, false if not
   */
  def balance(chars: List[Char]): Boolean = {
    @tailrec
    def balanceIter(chars: List[Char], leftExcess: Int): Boolean = {
      if (chars.isEmpty) // nothing left, did we come out even?
        leftExcess == 0
      else if (chars.head == '(')
        balanceIter(chars.tail, leftExcess + 1)
      else if (chars.head == ')' && leftExcess > 0)
        balanceIter(chars.tail, leftExcess - 1)
      else if (chars.head == ')') // got a right paren, but no excess remains!
        false
      else // not a paren
        balanceIter(chars.tail, leftExcess)
    }
    balanceIter(chars, 0)
  }

  /**
   * Exercise 3
   *
   * Compute the number of different ways change can be made for the amount `money` given
   * coin denominations listed in `coins`, which may be assumed distinct.  This function
   * uses the naive recursive algorithm, which takes time exponential in the length of
   * `coins` and does not attempt to memoize at all.
   * @param money Amount of money to make change for
   * @param coins List of distinct positive integers representing coin denominations
   * @return Number of different ways to do it
   */
  def countChange(money: Int, coins: List[Int]): Int = {
    if (money == 0)
      1
    else if (money < 0)
      0
    else if (coins.isEmpty)
      0
    else {
      val c = coins.head
      (0 to money / c).map(n => countChange(money - n * c, coins.tail)).reduce(_ + _)
    }
  }
}
