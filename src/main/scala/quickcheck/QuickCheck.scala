package quickcheck

import common._

import org.scalacheck._
import Arbitrary._
import Gen._
import Prop._

abstract class QuickCheckHeap extends Properties("Heap") with IntHeap {

  property("min1") = forAll { a: Int =>
    val h = insert(a, empty)
    findMin(h) == a
  }

  property("assoc1") = forAll { (a: Int, b: Int) =>
  	insert(a, insert(b, empty)) == insert(b, insert(a, empty))
  }

  property("assoc2") = forAll { (a: H, b: H, c: H) =>
    findMin(meld(meld(a, b), c)) == findMin(meld(a, meld(b, c)))
  }

  property("assoc3") = forAll { (a: Int, b: Int) =>
    meld(insert(a, empty), insert(b, empty)) == insert(a, meld(empty, insert(b, empty)))
  }

  lazy val genHeap: Gen[H] = for {
  	k <- arbitrary[Int]
  	m <- oneOf(empty, genHeap)
  } yield insert(k, m)

  implicit lazy val arbHeap: Arbitrary[H] = Arbitrary(genHeap)

}
