package quickcheck

import common._

import org.scalacheck._
import Arbitrary._
import Gen._
import Prop._

abstract class QuickCheckHeap extends Properties("Heap") with IntHeap {

  lazy val genHeap: Gen[H] = oneOf(
    const[H](empty),
    for {
      n <- arbitrary[A]
      heap <- oneOf(const[H](empty), genHeap)
    } yield insert(n, heap))
  implicit lazy val arbHeap: Arbitrary[H] = Arbitrary(genHeap)

  type ABTuple = (Int, Int)

  lazy val genABTuple: Gen[ABTuple] =
    for {
      b <- Gen.choose(Int.MinValue, Int.MaxValue - 10)
      a <- Gen.choose(b + 1, Int.MaxValue)
    } yield (a,b)

  implicit lazy val arbABTuple: Arbitrary[ABTuple] = Arbitrary(genABTuple)

  property("min1") = forAll { a: Int =>
    val h = insert(a, empty)
    findMin(h) == a
  }

  property("min2") = forAll { ab: ABTuple =>
    val(a, b) = ab
    val h = insert(b, insert(a, empty))
    findMin(h) == b
  }

  property("gen1") = forAll { h: H =>
    val m = if (isEmpty(h)) 0 else findMin(h)
    findMin(insert(m, h)) == m
  }

  property("delete one") = forAll { a: Int =>
    val h = insert(a, empty)
    deleteMin(h) == empty
  }

  property("minimums are sorted") = forAll { h: H => {
    def aux(mins: List[A], rest: H): Boolean =
      if (isEmpty(rest)) mins == mins.sorted
      else aux(mins :+ findMin(rest), deleteMin(rest))

      if (isEmpty(h)) true
      else aux(List(findMin(h)), deleteMin(h))
    }
  }

  property("ins sorted") = forAll { ab: ABTuple =>
    val(a, b) = ab
    val h = insert(b, insert(a, empty))
    val newB = findMin(h)
    val newA = findMin(deleteMin(h))
    List(b, a) == List(newB, newA)
  }

  property("deleteMin") = forAll { ab: ABTuple =>
    val (a, b) = ab
    val bs = insert(b, insert(b, empty))
    val h = insert(a, bs)
    val withoutOneB = deleteMin(h)
    val newB = findMin(withoutOneB)
    val withoutBs = deleteMin(withoutOneB)
    val newA = findMin(withoutBs)
    newB == b && newA == a
  }

  property("min of melded2") = forAll { ab: ABTuple =>
    val(a, b) = ab
    val h1 = insert(a, empty)
    val h2 = insert(b, empty)
    val melded = meld(h1, h2)
    findMin(melded) == b
  }
}
