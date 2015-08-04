package org.invisibletech.myfpscala

import org.invisibletech.myfpscala._
import org.scalatest._

class ListSpec extends FlatSpec with Matchers {
  "List.add1" should "add 1 to each element" in {
    val list: List[Int] = List(1, 0, 10, -1)
    val listUp1: List[Int] = List.add1(list)

    listUp1 match {
      case Nil => fail
      case Cons(h, _) => h should be(2)
    }

    List.reverse(listUp1) match {
      case Nil => fail
      case Cons(h, _) => h should be (0)
    }
  }

  "List.doubleToString" should "convert List of Doubles to a list of Strings" in {
    val list: List[Double] = List(2.4, 0.5, 4.3)
    val listStr: List[String] = List.doubleToString(list)

    listStr match {
      case Nil => fail
      case Cons(h, _) => h should be("2.4")
    }

    List.reverse(listStr) match {
      case Nil => fail
      case Cons(h, _) => h should be ("4.3")
    }
  }
    
  "List.map" should "convert List of Doubles to a list of Strings" in {
    val list: List[Double] = List(2.4, 0.5, 4.3)
    val listStr: List[String] = List.map(list)(_.toString)

    listStr match {
      case Nil => fail
      case Cons(h, _) => h should be("2.4")
    }

    List.reverse(listStr) match {
      case Nil => fail
      case Cons(h, _) => h should be ("4.3")
    }
  }

  "List.map" should "map should add 1 to each element" in {
    val list: List[Int] = List(1, 0, 10, -1)
    val listUp1: List[Int] = List.map(list)(_ + 1)

    listUp1 match {
      case Nil => fail
      case Cons(h, _) => h should be(2)
    }

    List.reverse(listUp1) match {
      case Nil => fail
      case Cons(h, _) => h should be (0)
    }
  }

}