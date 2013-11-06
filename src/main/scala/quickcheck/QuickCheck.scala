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

  property("assoc") = forAll { (a: Int, b: Int) =>
  	insert(a, insert(b, empty)) == insert(b, insert(a, empty))
  }
  // insert(a, empty) meld insert(b, empty) == insert(a, insert(b, empty))
  lazy val genHeap: Gen[H] = for {
  	k <- arbitrary[Int]
  	m <- oneOf(empty, genHeap)
  } yield m.insert(k, m)

  lazy val genMap: Gen[Map[Int,Int]] = for {
    k <- arbitrary[Int]
    v <- arbitrary[Int]
    m <- oneOf(value(Map.empty[Int,Int]), genMap)
  } yield m.updated(k, v)

  implicit lazy val arbHeap: Arbitrary[H] = Arbitrary(genHeap)

}
