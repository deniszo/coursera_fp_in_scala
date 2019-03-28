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
    def pascal(c: Int, r: Int): Int =
      if (c <= 0 || r <= 0 || c == r) {
        1
      } else {
        pascal(c, r -1) + pascal(c-1, r - 1)
      }
  
  /**
   * Exercise 2
   */
    def balance(chars: List[Char]): Boolean = {
      def matchP(parens: List[Char], rawChars: List[Char]): Boolean = {
        if (rawChars.isEmpty) {
          parens.isEmpty
        } else {
          rawChars.head match {
            case '(' => matchP(rawChars.head :: parens, rawChars.tail)
            case ')' =>
              if (parens.isEmpty) {
                false
              } else {
                matchP(parens.tail, rawChars.tail)
              }
            case _ => matchP(parens, rawChars.tail)
          }
        }
      }

      matchP(List[Char](), chars)
    }
  
  /**
   * Exercise 3
   */
    def countChange(money: Int, coins: List[Int]): Int = {
      def count(waysNum: Int, money: Int, coins: List[Int]): Int =
        if (money <= 0 || coins.isEmpty) {
          waysNum
        } else if (coins.head > money) {
          count(waysNum, money, coins.tail)
        } else if (coins.head == money) {
          count(waysNum + 1, money, coins.tail)
        } else {
          count(waysNum, money - coins.head, coins) + count(0, money, coins.tail)
        }

      count(0, money, coins)
    }
  }
