package quickcheck

import common._

import org.scalacheck._
import Arbitrary._
import Gen._
import Prop._

abstract class QuickCheckHeap extends Properties("Heap") with IntHeap {

  lazy val genHeap: Gen[H] = oneOf(
    const(empty),
    for {
      v <- arbitrary[Int]
      h <- genHeap
    } yield insert(v, h)
  )

  implicit lazy val arbHeap: Arbitrary[H] = Arbitrary(genHeap)

  property("min1") = forAll { a: Int =>
    val h = insert(a, empty)
    findMin(h) == a
  }

  property("min2") = forAll { (a: Int, b: Int) =>
    val h = insert(b, insert(a, empty))
    findMin(h) == (a min b)
  }

  property("gen1") = forAll { (h: H) =>
    val m = if (isEmpty(h)) 0 else findMin(h)
    findMin(insert(m, h)) == m
  }

  property("delete1") = forAll { a: Int =>
    val h = deleteMin(insert(a, empty))
    isEmpty(h)
  }

  property("minMeldNonEmpty") = forAll { (h1: H, h2: H) =>
    (!isEmpty(h1) & !isEmpty(h2)) ==> {
      val h = meld(h1, h2)
      findMin(h) == (findMin(h1) min findMin(h2))
    }
  }

  property("minMeld") = forAll { (h1: H, h2: H) =>
    (!isEmpty(h1) | !isEmpty(h2)) ==> {
      val minVal =
        if (isEmpty(h1)) findMin(h2)
        else if (isEmpty(h2)) findMin(h1)
        else findMin(h2) min findMin(h1)
      val h = meld(h1, h2)
      findMin(h) == minVal
    }
  }

  property("retSorted") = forAll { h: H =>
    def get(heap: H, xs: List[Int]): List[Int] = {
      if (isEmpty(heap)) xs.reverse
      else {
        val x = findMin(heap)
        get(deleteMin(heap), x :: xs)
      }
    }
    val resList = get(h, List())
    resList == resList.sorted
  }
}
