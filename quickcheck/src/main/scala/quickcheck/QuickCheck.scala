package quickcheck

import org.scalacheck.*
import Arbitrary.*
import Gen.*
import Prop.forAll

abstract class QuickCheckHeap extends Properties("Heap") with IntHeap:
  lazy val genHeap: Gen[H] = oneOf(
    const(empty),
    for {
      n <- arbitrary[A]
      m <- genHeap.sample match
        case Some(h: H) => insert(n, h)
        case None => insert(n, empty)
    } yield m
  )
  given Arbitrary[H] = Arbitrary(genHeap)

  property("min1") = forAll { (a: A) =>
    val h = insert(a, empty)
    findMin(h) == a
  }

  property("gen1") = forAll { (h: H) =>
    val m = if isEmpty(h) then 0 else findMin(h)
    findMin(insert(m, h)) == m
  }

  property("min2") = forAll { (a: A, b: A) =>
    val cond = if a < b then a else b
    findMin(insert(a, insert(b, empty))) == cond
  }

  property("mindel") = forAll { (a: A) =>
    deleteMin(insert(a, empty)) == empty
  }

  property("sortedseq") = forAll { (h: H) =>
    def sortedHeap(h: H): H = 
      if isEmpty(h) then return empty
      else 
        val m = findMin(h)        
        val nh = deleteMin(h)     
        insert(m, sortedHeap(nh))
    
    def pred(h: H): H = 
      if h == empty || deleteMin(h) == empty then return empty
      else 
        val m = findMin(h)     
        if m <= findMin(deleteMin(h)) then pred(deleteMin(h)) else h   

    pred(sortedHeap(h)) == empty
  }

  property("mintwoheaps") = forAll { (h1: H, h2: H) =>
    if isEmpty(h1) && isEmpty(h2) then
      true
    else
      val min = findMin(meld(h1, h2))
      (!isEmpty(h1) && min == findMin(h1)) || (!isEmpty(h2) && min == findMin(h2))
  }
  
  property ("mindel2") = forAll { (x: A, y: A, z: A) =>
    val max = Math.max(x, Math.max(y, z))
    val heap = meld(insert(z, empty), meld(insert(x, empty), insert(y,empty)))
    findMin(deleteMin(deleteMin(heap))) == max
  }







