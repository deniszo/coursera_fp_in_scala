package playgrounds

object CombSearchAndForExpr {
  def isPrime(n: Int) = (2 until n) forall (n % _ != 0)

  def main(args: Array[String]): Unit = {
    // Given a positive integer n, find all pairs of positive integers
    // i and j, with 1 <= j < i < n such that i + j is prime

    val n = 7

    // we get what we want but it might be hard to parse
    println((1 until n) flatMap (i =>
      (1 until i) map (j => (i, j))) filter { case (x, y) =>
        isPrime(x + y) } )

    // .. so for expressions come to the rescue
    println(
      for {
        i <- 1 until n
        j <- 1 until i
        if isPrime(i + j)
      } yield (i, j)
    )

    // scalarFor rewritten with for
    val xs = 1 to 5
    val ys = 1 to 4

    println(
      (for ((x, y) <- xs zip ys) yield x * y).sum
    )

    // N-Queens
    // place eight queens on a chessboard so that
    // no queen is threatened by another
    def queens(n: Int): Set[List[Int]] = {
      def isSafe(col: Int, queens: List[Int]): Boolean = {
        val row = queens.length
        val queensWithRow = (row - 1 to 0 by -1) zip queens
        queensWithRow forall {
          case (r, c) => col != c && math.abs(col - c) != row - r
        }
      }

      def placeQueens(k: Int): Set[List[Int]] =
        if (k == 0) Set(List())
        else
          for {
            queens <- placeQueens(k - 1)
            col <- 0 until n
            if isSafe(col, queens)
          } yield col :: queens
      placeQueens(n)
    }

    def show(queens: List[Int]) = {
      val lines =
        for (col <- queens.reverse)
          yield Vector.fill(queens.length)("* ").updated(col, "X ").mkString
      "\n" + (lines mkString "\n")
    }

    println("queens(4)", queens(4) map show)
  }
}
