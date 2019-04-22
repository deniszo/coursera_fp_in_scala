package playgrounds

object Maps {
  class Poly(terms0: Map[Int, Double]) {
    def this(bindings: (Int, Double)*) = this(bindings.toMap)
    val terms = terms0 withDefaultValue 0.0
    def + (other: Poly) = new Poly((other.terms foldLeft terms)(addTerm))

    def addTerm(terms: Map[Int, Double], term: (Int, Double)): Map[Int, Double] = {
      val (exp, coeff) = term
      terms + (exp -> (coeff + terms(exp)))
    }

    override def toString: String =
      (for((exp, coeff) <- terms.toList.sorted.reverse) yield coeff + "x^" + exp) mkString " + "
  }

  def main(args: Array[String]): Unit = {
    val capitalOfCountry = Map("US" -> "Washington", "Switzerland" -> "Bern")

    // maps can be used everywhere functions can
    println(capitalOfCountry("US"))
    // throws
    // println(capitalOfCountry("Andorra"))
    // but thi does not
    println(capitalOfCountry get "Andorra")

    val fruit = List("apple", "pear", "orange", "pineapple")
    println(fruit sortWith (_.length < _.length))
    println(fruit.sorted)

    println(fruit groupBy (_.head))

    val p1 = new Poly(1 -> 2.0, 3 -> 4.0, 5 -> 6.2)
    val p2 = new Poly(0 -> 3.0, 3 -> 7.0)

    println(p1 + p2)
  }
}
