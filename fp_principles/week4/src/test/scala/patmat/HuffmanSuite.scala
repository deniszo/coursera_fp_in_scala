package patmat

import org.scalatest.FunSuite

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

import patmat.Huffman._

@RunWith(classOf[JUnitRunner])
class HuffmanSuite extends FunSuite {
	trait TestTrees {
		val t1 = Fork(Leaf('a',2), Leaf('b',3), List('a','b'), 5)
		val t2 = Fork(Fork(Leaf('a',2), Leaf('b',3), List('a','b'), 5), Leaf('d',4), List('a','b','d'), 9)
	}


  test("weight of a larger tree") {
    new TestTrees {
      assert(weight(t1) === 5)
    }
  }


  test("chars of a larger tree") {
    new TestTrees {
      assert(chars(t2) === List('a','b','d'))
    }
  }


  test("string2chars(\"hello, world\")") {
    assert(string2Chars("hello, world") === List('h', 'e', 'l', 'l', 'o', ',', ' ', 'w', 'o', 'r', 'l', 'd'))
  }

  test("times(List('a', 'b', 'c', 'a')") {
    assert(times(List('a', 'b', 'c', 'a')) === List(('a', 2), ('b', 1), ('c', 1)))
  }

  test("times(List('b', 'b', 'b', 'a', 'z', 'z', 'a')") {
    assert(times(List('b', 'b', 'b', 'a', 'z', 'z', 'a')) === List(('b', 3), ('a', 2), ('z', 2)))
  }

  test("makeOrderedLeafList for some frequency table") {
    assert(makeOrderedLeafList(List(('t', 2), ('e', 1), ('x', 3))) === List(Leaf('e',1), Leaf('t',2), Leaf('x',3)))
  }


  test("combine of some leaf list") {
    val leaflist = List(Leaf('e', 1), Leaf('t', 2), Leaf('x', 4))
    assert(combine(leaflist) === List(Fork(Leaf('e',1),Leaf('t',2),List('e', 't'),3), Leaf('x',4)))
  }

  test("until(singleton, combine)(trees)") {
    val trees = List(Leaf('e', 1), Leaf('t', 2), Leaf('x', 3))
    assert(until(singleton, combine)(trees) === List(Fork(Fork(Leaf('e', 1), Leaf('t', 2), List('e', 't'), 3), Leaf('x', 3), List('e', 't', 'x'), 6)))
  }

  test("createCodeTree(List('e', 't', 'x', 'x', 't', 'x'))") {
    assert(createCodeTree(List('e', 't', 'x', 'x', 't', 'x')) === Fork(Fork(Leaf('e', 1), Leaf('t', 2), List('e', 't'), 3), Leaf('x', 3), List('e', 't', 'x'), 6))
  }

  test("decode very short text") {
    new TestTrees {
      assert(decode(t1, List(0, 1)) === "ab".toList)
    }
  }

  test("encode a very short text") {
    new TestTrees {
      assert(encode(t1)("ab".toList) === List(0, 1))
    }
  }

  test("decode and encode a very short text should be identity") {
    new TestTrees {
      assert(decode(t1, encode(t1)("ab".toList)) === "ab".toList)
    }
  }

  test("codeBits finds bits") {
    new TestTrees {
      assert(codeBits(List(('a', List(1)), ('b', List(1, 2, 3))))('b') === List(1, 2, 3))
    }
  }

  test("create code tree builds an optimal tree") {
    new TestTrees {
      val inputList = List('b', 'a', 'a', 'b', 'c')
      val timeS = times(inputList)
      assert(timeS === List(('b', 2), ('a', 2), ('c', 1)))
      assert(makeOrderedLeafList(timeS) == List(Leaf('c', 1), Leaf('b', 2), Leaf('a', 2)))
      assert(createCodeTree(inputList) ==
        Fork(
          Leaf('a', 2),
          Fork(
            Leaf('c', 1),
            Leaf('b', 2),
            List('c', 'b'),
            3
          ),
          List('a', 'c', 'b'),
          5
        )
      )
    }
  }

  test("createCodeTree(someText)") {
    new TestTrees {
      assert(makeOrderedLeafList(times("someText".toList)) == List(Leaf('s', 1), Leaf('o', 1), Leaf('m', 1), Leaf('T', 1), Leaf('x', 1), Leaf('t', 1), Leaf('e', 2)))
      assert(createCodeTree("someText".toList) ==
        Fork(
          Fork(
            Fork(
              Leaf('s', 1),
              Leaf('o', 1),
              List('s', 'o'),
              2
            ),
            Leaf('e', 2),
            List('s', 'o', 'e'),
            4
          ),
          Fork(
            Fork(
              Leaf('x', 1),
              Leaf('t', 1),
              List('x', 't'),
              2
            ),
            Fork(
              Leaf('m', 1),
              Leaf('T', 1),
              List('m', 'T'),
              2
            ),
            List('x', 't', 'm', 'T'),
            4
          ),
          List('s', 'o', 'e', 'x', 't', 'm', 'T'),
          8
        )
      )
    }
  }

  test("convert does its job") {
    new TestTrees {
      assert(convert(
        Fork(
          Fork(
            Leaf('e',1),
            Leaf('t',2),
            List('e', 't'),
            3
          ),
          Leaf('a',2),
          List('e', 't', 'a'),
          5
        )
      ) === List(('e', List(0, 0)), ('t', List(0, 1)), ('a', List(1))))
    }
  }

  test("quickEncode a very short text") {
    new TestTrees {
      assert(quickEncode(t1)("ab".toList) === List(0, 1))
    }
  }
}
