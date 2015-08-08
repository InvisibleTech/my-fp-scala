package org.invisibletech.myfpscala


import org.invisibletech.myfpscala._
import org.scalatest._

class TreeSpec extends FlatSpec with Matchers {
  "Branch constructor" should "accept just leaves" in {
    val minitree = Branch(Leaf(10), Leaf(8))

    minitree match {
      case Branch(Leaf(left), Leaf(right)) => (left, right) shouldBe (10, 8)
    }
  }

  "Branch constructor" should "accept just branches and leaves" in {
    val minitree = Branch(Branch(Leaf(10), Leaf(9)), Leaf(8))

    minitree match {
      case Branch(Branch(Leaf(left), Leaf(middle)), Leaf(right)) => (left, middle, right) shouldBe (10, 9, 8)
    }
  }
}