package isort

object test extends App {
  def insert(x: Int, xs: List[Int]): List[Int] = xs match {
      case List() => List(x)
      case y :: ys => {
        if (x < y) x :: xs
        else y :: insert(x, ys)
      }
  }


  def isort(xs: List[Int]): List[Int] = xs match {
    case List() => List()
    case y :: ys => insert(y, isort(ys))
  }

  override def main(args: Array[String]): Unit = {
    println(isort(List(9, 5, 6, 1, 3)))
  }
}