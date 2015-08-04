package org.invisibletech.myfpscala

import org.invisibletech.myfpscala._
import org.scalatest._

class ListSpec extends FlatSpec with Matchers {
  "List.add1" should "add 1 to each element" in {
    val list: List[Int] = List(1, 0, 10, -1)
    val listUp1: List[Int] = List.add1(list)

    listUp1 should be (List(2, 1, 11, 0))
  }

  "List.doubleToString" should "convert List of Doubles to a list of Strings" in {
    val list: List[Double] = List(2.4, 0.5, 4.3)
    val listStr: List[String] = List.doubleToString(list)

    listStr should be (List("2.4", "0.5", "4.3"))
  }
    
  "List.map" should "convert List of Doubles to a list of Strings" in {
    val list: List[Double] = List(2.4, 0.5, 4.3)
    val listStr: List[String] = List.map(list)(_.toString)

    listStr should be (List("2.4", "0.5", "4.3"))
  }

  "List.map" should "should add 1 to each element" in {
    val list: List[Int] = List(1, 0, 10, -1)
    val listUp1: List[Int] = List.map(list)(_ + 1)

    listUp1 should be (List(2, 1, 11, 0))
  }

  "List.filter" should "should return Nil for Nil" in {
    val filteredList: List[Int] = List.filter(Nil:List[Int])(_ != 0)

    filteredList should be(Nil)
  }

  "List.filter" should "should return Nil for list that is completely filtered" in {
    val filteredList: List[Int] = List.filter(List(0, 0, 0))(_ != 0)

    filteredList should be(Nil)
  }

  "List.filter" should "should return List that has been fitered" in {
    val filteredList: List[Int] = List.filter(List(10, 0, 10))(_ != 0)

    filteredList should be(List(10, 10))
  }
}