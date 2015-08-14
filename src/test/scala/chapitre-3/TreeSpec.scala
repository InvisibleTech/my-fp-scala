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

  "size" should "return 15 for a balanced tree 4 levels deep" in {
    Tree.size(Branch(Branch(Branch(Leaf(5.4), Leaf(100.4)), Branch(Leaf(-5.4), Leaf(-100.4))), Branch(Branch(Leaf(665.4), Leaf(1009.4)), Branch(Leaf(-50.4), Leaf(10.4))))) shouldBe (15)
  }

  "size" should "return 5 for an unbalanced tree" in  {
    Tree.size(Branch(Branch(Leaf(-5.4), Leaf(-100.4)), Leaf(1009.4))) shouldBe (5)
  }

  "max" should "return 1 for a one level tree with all 1s" in {
    Tree.max(Branch(Leaf(1), Leaf(1))) shouldBe (1)
  }

  "max" should "return 5000 for a balanced tree with leftmost value of 5000" in {
    Tree.max(Branch(Branch(Branch(Leaf(5000), Leaf(100)), Branch(Leaf(-5), Leaf(-100))), Branch(Branch(Leaf(665), Leaf(1009)), Branch(Leaf(-50), Leaf(10))))) shouldBe (5000)
  }

  "max" should "return 5000 for a balanced tree with rightmost value of 5000" in {
    Tree.max(Branch(Branch(Branch(Leaf(0), Leaf(100)), Branch(Leaf(-5), Leaf(-100))), Branch(Branch(Leaf(665), Leaf(1009)), Branch(Leaf(-50), Leaf(5000))))) shouldBe (5000)
  }

  "max" should "return 5000 for a balanced tree with inner value of 5000" in {
    Tree.max(Branch(Branch(Branch(Leaf(0), Leaf(100)), Branch(Leaf(-5), Leaf(-100))), Branch(Branch(Leaf(665), Leaf(5000)), Branch(Leaf(-50), Leaf(300))))) shouldBe (5000)
  }

  "depth" should "return 3 for a balanced tree with a maximum of 3 branches and a leaf" in {
    Tree.depth(Branch(Branch(Branch(Leaf(5.4), Leaf(100.4)), Branch(Leaf(-5.4), Leaf(-100.4))), Branch(Branch(Leaf(665.4), Leaf(1009.4)), Branch(Leaf(-50.4), Leaf(10.4))))) shouldBe (3)
  }

  "depth" should "return 3 for an imbalanced tree with a maximum of 3 branches and a leaf" in {
    Tree.depth(Branch(Branch(Leaf(5.4), Leaf(-100.4)), Branch(Leaf(-50.4), Branch(Leaf(-5.4), Leaf(-100.4))))) shouldBe (3)
  }

  "map" should "return a tree of unmodified Leaves gievn an identity" in {
    Tree.map(Branch(Leaf("a"), Branch(Leaf("b"), Branch(Leaf("g"), Leaf("h")))))((x) => x) shouldBe (Branch(Leaf("a"), Branch(Leaf("b"), Branch(Leaf("g"), Leaf("h")))))
  }
}