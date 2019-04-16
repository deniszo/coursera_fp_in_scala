package pm

trait Expr
case class Number(n: Int) extends Expr
case class Var(name: String) extends Expr
case class Sum(e1: Expr, e2: Expr) extends Expr
case class Prod(e1: Expr, e2: Expr) extends Expr

object exprs extends App {
  def show(e: Expr): String = e match {
    case Number(x) => x.toString
    case Var(name) => name
    case Sum(l, r) => show(l) + " + " + show(r)
    case Prod(l, r) => {
        def placeParens(e: Expr): String = e match {
          case Sum(_, _) => "(" + show(e) + ")"
          case _ => show(e)
        }

        placeParens(l) + " * " + placeParens(r)
    }
  }

  override def main(args: Array[String]): Unit = {
    println(show(Sum(Number(1), Number(44))))

    println(show(Sum(Prod(Number(2), Var("x")), Var("y"))))

    println(show(Prod(Sum(Number(2), Var("x")), Var("y"))))
  }
}