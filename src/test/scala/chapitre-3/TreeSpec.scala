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

  "Branch constructor" should "allow for null leaves" in {
    val minitree = Branch(null, null)

    minitree match {
      case Branch(left, right) => (left, right) shouldBe (null, null)
    }
  }

  "size" should "return 0 for a null tree" in {
    Tree.size(null) shouldBe (0)
  }

  "size" should "return 1 for a tree with null branches" in {
    Tree.size(Branch(null, null)) shouldBe (1)
  }

  "size" should "return 2 for a tree with one left leaf" in {
    Tree.size(Branch(Leaf("e"), null)) shouldBe (2)
  }

  "size" should "return 2 for a tree with one right leaf" in {
    Tree.size(Branch(null, Leaf(9))) shouldBe (2)
  }

  "size" should "return 4 for a tree with left only branches and a leaf 4 deep" in {
    Tree.size(Branch(Branch(Branch(Leaf(5.4), null), null), null)) shouldBe (4)
  }

  "size" should "return 4 for a tree with right only branches and a leaf 4 deep" in {
    Tree.size(Branch(null, Branch(null, Branch(null, Leaf(5.4))))) shouldBe (4)
  }

  "size" should "return 15 for a balanced tree 4 levels deep" in {
    Tree.size(Branch(Branch(Branch(Leaf(5.4), Leaf(100.4)), Branch(Leaf(-5.4), Leaf(-100.4))), Branch(Branch(Leaf(665.4), Leaf(1009.4)), Branch(Leaf(-50.4), Leaf(10.4))))) shouldBe (15)
  }

}