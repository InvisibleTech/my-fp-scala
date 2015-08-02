package org.invisibletech.myfpscala

import org.invisibletech.myfpscala._
import org.scalatest._

class ListSpec extends FlatSpec with Matchers {
  "List" should "add 1 to each element" in {
    val list: List[Int] = List(1, 0, 10, -1)
    val listUp1: List[Int] = List.add1(list)

    listUp1 match {
      case Nil => fail
      case Cons(h, _) => h should be(2)
    }

    List.reverse(listUp1) match {
      case Nil => fail
      case Cons(h, _) => h should be (70)
    }
  }
    
}