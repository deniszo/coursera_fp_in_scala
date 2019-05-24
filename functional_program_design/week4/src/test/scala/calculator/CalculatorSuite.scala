package calculator

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import org.scalatest.{FunSuite, _}

import TweetLength.MaxTweetLength

@RunWith(classOf[JUnitRunner])
class CalculatorSuite extends FunSuite with ShouldMatchers {

  /******************
   ** TWEET LENGTH **
   ******************/

  def tweetLength(text: String): Int =
    text.codePointCount(0, text.length)

  test("tweetRemainingCharsCount with a constant signal") {
    val result = TweetLength.tweetRemainingCharsCount(Var("hello world"))
    assert(result() == MaxTweetLength - tweetLength("hello world"))

    val tooLong = "foo" * 200
    val result2 = TweetLength.tweetRemainingCharsCount(Var(tooLong))
    assert(result2() == MaxTweetLength - tweetLength(tooLong))
  }

  test("tweetRemainingCharsCount with a supplementary char") {
    val result = TweetLength.tweetRemainingCharsCount(Var("foo blabla \uD83D\uDCA9 bar"))
    assert(result() == MaxTweetLength - tweetLength("foo blabla \uD83D\uDCA9 bar"))
  }


  test("colorForRemainingCharsCount with a constant signal") {
    val resultGreen1 = TweetLength.colorForRemainingCharsCount(Var(52))
    assert(resultGreen1() == "green")
    val resultGreen2 = TweetLength.colorForRemainingCharsCount(Var(15))
    assert(resultGreen2() == "green")

    val resultOrange1 = TweetLength.colorForRemainingCharsCount(Var(12))
    assert(resultOrange1() == "orange")
    val resultOrange2 = TweetLength.colorForRemainingCharsCount(Var(0))
    assert(resultOrange2() == "orange")

    val resultRed1 = TweetLength.colorForRemainingCharsCount(Var(-1))
    assert(resultRed1() == "red")
    val resultRed2 = TweetLength.colorForRemainingCharsCount(Var(-5))
    assert(resultRed2() == "red")
  }

  test("computeDelta") {
    val deltaSignal = Polynomial.computeDelta(Var(2), Var(3), Var(4))
    assert(deltaSignal() == -23.0)
  }

  test("computeSolutions no roots") {
    val (a, b, c) = (Var(2.0), Var(3.0), Var(4.0))
    val compute = Polynomial.computeSolutions(a, b, c, Polynomial.computeDelta(a, b, c))
    val result: Set[Double] = compute()
    assert(result.isEmpty)
  }

  test("computeSolutions 1 root") {
    val (a, b, c) = (Var(1.0), Var(2.0), Var(1.0))
    val compute = Polynomial.computeSolutions(a, b, c, Polynomial.computeDelta(a, b, c))
    val result: Set[Double] = compute()
    assert(result.size == 1)
    assert(result.contains(-1.0))
  }

  test("computeSolutions 2 roots") {
    val (a, b, c) = (Var(5.0), Var(6.0), Var(1.0))
    val compute = Polynomial.computeSolutions(a, b, c, Polynomial.computeDelta(a, b, c))
    val result: Set[Double] = compute()
    assert(result.size == 2)
    assert(result.contains(-0.2))
    assert(result.contains(-1.0))
  }

  test("eval Ref('a') when a is missing returns NaN") {
    assert(Double.box(Calculator.eval(Ref("a"), Map.empty)).isNaN)
  }

  test("computeValues with self ref sets self ref key values to NaN") {
    Calculator.computeValues(Map("a" -> Var(Ref("a")), "b" -> Var(Ref("b")))) foreach { case(_, v) =>
      assert(Double.box(v()).isNaN)
    }
  }

  test("computeValues with circular refs values to NaN") {
    val res = Calculator.computeValues(
      Map(
        "a" -> Var(Plus(Ref("b"), Literal(1.0))),
        "b" -> Var(Times(Literal(2.0), Ref("a")))
      ))

    res foreach { case(key, v) =>
      println(s"$key is ${v()}")
      assert(Double.box(v()).isNaN)
    }
  }

  test("computeValues simple") {
    val res = Calculator.computeValues(
      Map(
        "a" -> Var(Plus(Ref("b"), Literal(1.0))),
        "b" -> Var(Times(Literal(2.0), Ref("c"))),
        "c" -> Var(Literal(4.0))
      ))

    val a = res getOrElse("a", Var(Double.NaN))
    assert(a() == 9.0)
  }
}
