package playgrounds

object OtherCollections {
  def main(args: Array[String]): Unit = {
    val xs = Array(1, 2, 3, 44)
    println(xs map (x => x * 2))

    val s = "Hello World"
    println(s filter (s => s.isUpper))

    val r1: Range = 1 until 5
    println(r1)

    val r2: Range = 1 to 5
    println(r2)

    println(1 to 10 by 3)
    println(6 to 1 by -2)

    // more operations
    println("s exists (c => c.isUpper)", s exists (c => c.isUpper))

    println("s forall (c => c.isUpper)", s forall (c => c.isUpper))

    println("List(1, 2, 3) zip s", List(1, 2, 3) zip s)

    println("s flatMap (c => List('.', c)", s flatMap (c => List('.', c)))

    println("xs.sum", xs.sum)

    // List all combinations of numbers x and y where x is drawn
    // from 1..M and y is drawn from 1..N

    val M = 5
    val N = 4

    println((1 to M) flatMap(x => (1 to N) map (y => (x, y))))

    // scalar product of two vectors
    def scalarProduct(xs: Vector[Double], ys: Vector[Double]): Double =
      (xs zip ys).map{ case (x, y) => x * y }.sum
    println("scalarProduct", scalarProduct(Vector(2, 4), Vector(1, 3)))

    def isPrime(n: Int): Boolean = (2 until n) forall (d => n % d != 0)

    println("7 is prime?", isPrime(7))

    // Sets
    val fruit = Set("apple", "banana", "pear")
    println(fruit filter (_.startsWith("app")))
  }
}
