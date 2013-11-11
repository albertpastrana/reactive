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

  //If you insert an element into an empty heap, then delete the minimum,
  //the resulting heap should be empty.
  property("empty") = forAll { a: Int =>
    val h = insert(a, empty)
    deleteMin(h) == empty
  }

  //If you insert any two elements into an empty heap, finding the minimum of the resulting heap
  //should get the smallest of the two elements back.
  property("rightmin") = forAll { (a: Int, b: Int) =>
    val min = Math.min(a, b)
    min == findMin(insert(a, insert(b, empty)))
  }

  property("commut") = forAll { (a: H, b: H) =>
    equals(meld(a, b), meld(b, a))
  }

  property("assoc1") = forAll { (a: Int, b: Int) =>
    insert(a, insert(b, empty)) == insert(b, insert(a, empty))
  }

  property("assoc2") = forAll { (a: H, b: H, c: H) =>
    equals(meld(meld(a, b), c), (meld(a, meld(b, c))))
  }

  property("assoc3") = forAll { (a: Int, b: Int) =>
    equals(meld(insert(a, empty), insert(b, empty)), insert(a, meld(empty, insert(b, empty))))
  }

  //Given any heap, you should get a sorted sequence of elements when continually finding
  //and deleting minima. (Hint: recursion and helper functions are your friends.)
  property("ordered") = forAll { (a: H) =>
    def isOrdered(previous: Int, ts: H):Boolean = { 
      if (isEmpty(ts))
        true
      else {
        val min = findMin(ts)
        if (previous > min) false
        else isOrdered(min, deleteMin(ts))
      }
    }
    val min = findMin(a)
    isOrdered(min, deleteMin(a))
  }

  property("ordered1") = forAll { (a: Int, b: Int) =>
    val (first, second) = if (a < b) (a, b) else (b, a)
    val h = insert(a, insert(b, empty))
    findMin(h) == first
    val h1 = deleteMin(h)
    findMin(h1) == second
  }

  //Finding a minimum of the melding of any two heaps should return a minimum of one or the other.
  property("min2heaps") = forAll { (a: H, b: H) =>
    val mina = findMin(a)
    val minb = findMin(b)
    val expected = Math.min(mina, minb)
    expected == findMin(meld(a, b))
  }

  //Finding a minimum of the melding of any 3 heaps should return a minimum of one or the other.
  property("min3heaps") = forAll { (a: H, b: H, c: H) =>
    val mina = findMin(a)
    val minb = findMin(b)
    val minc = findMin(c)
    val expected = Math.min(Math.min(mina, minb), minc)
    expected == findMin(meld(meld(a, b), c))
  }

  property("size2->empty") = forAll { (a: Int) =>
    val h = insert(a, insert(a, empty))
    deleteMin(deleteMin(h)) == empty
  }

  def equals(a: H, b: H):Boolean = { 
    if (isEmpty(a) && isEmpty(b))
      true
    else if (isEmpty(a) && !isEmpty(b))
      false
    else if (!isEmpty(a) && isEmpty(b))
      false
    else {
      val mina = findMin(a)
      val minb = findMin(b)
      if (mina != minb) false
      else equals(deleteMin(a), deleteMin(b))
    }
  }

  lazy val genHeap: Gen[H] = for {
  	k <- arbitrary[Int]
  	m <- oneOf(empty, genHeap, genHeap, genHeap, genHeap, genHeap, genHeap, genHeap, genHeap)
  } yield insert(k, m)

  implicit lazy val arbHeap: Arbitrary[H] = Arbitrary(genHeap)

}
