package calculator

import scala.math.{pow, sqrt}

object Polynomial {
  def computeDelta(a: Signal[Double], b: Signal[Double],
      c: Signal[Double]): Signal[Double] = {
    Var(
      pow(b(), 2) - 4 * a() * c()
    )
  }

  def computeSolutions(a: Signal[Double], b: Signal[Double],
      c: Signal[Double], delta: Signal[Double]): Signal[Set[Double]] = {
    Var({
      if (delta() < 0) Set()
      else (-b(), sqrt(delta()), 2 * a()) match {
        case (minusB, sqrtD, _2a) =>
          if (sqrtD == 0)  Set(minusB / _2a)
          else Set((minusB + sqrtD) / _2a, (minusB - sqrtD) / _2a)
      }
    })

  }
}
