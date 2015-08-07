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

  "List.map" should "add 1 to each element" in {
    val list: List[Int] = List(1, 0, 10, -1)
    val listUp1: List[Int] = List.map(list)(_ + 1)

    listUp1 should be (List(2, 1, 11, 0))
  }

  "List.filter" should "return Nil for Nil" in {
    val filteredList: List[Int] = List.filter(Nil:List[Int])(_ != 0)

    filteredList should be(Nil)
  }

  "List.filter" should "return Nil for list that is completely filtered" in {
    val filteredList: List[Int] = List.filter(List(0, 0, 0))(_ != 0)

    filteredList should be(Nil)
  }

  "List.filter" should "return List that has been fitered" in {
    val filteredList: List[Int] = List.filter(List(10, 0, 10))(_ != 0)

    filteredList should be(List(10, 10))
  }

  "List.flatMap" should "return a list that is flattened at the first level" in {
    val flattened = List.flatMap(List(List(1, 2, 3), List(1, 33, 44)))((x:List[Int]) => x)

    flattened should be(List(1, 2, 3, 1, 33, 44))
  }

  "List.flatFilter" should "return Nil for Nil" in {
    val filteredList: List[Int] = List.flatFilter(Nil:List[Int])(_ != 0)

    filteredList should be(Nil)
  }

  "List.flatFilter" should "return Nil for list that is completely filtered" in {
    val filteredList: List[Int] = List.flatFilter(List(0, 0, 0))(_ != 0)

    filteredList should be(Nil)
  }

  "List.flatFilter" should "return List that has been fitered" in {
    val filteredList: List[Int] = List.flatFilter(List(10, 0, 10))(_ != 0)

    filteredList should be(List(10, 10))
  }

  "List.zipWith" should "return a List that was zipped using additon given two lists of Ints" in {
    val z: List[Int] = List.zipWith(List(1, 2, 3), List(6, 5, 4))(_ + _)

    z should be(List(7, 7, 7))
  }

  "List.zipWith" should "return a List that was zipped List() for List of Int and List of String" in {
    val z: List[(Int, String)] = List.zipWith(List(1, 2, 3), List("f", "dog", "hh"))((_, _))

    z should be(List((1, "f"), (2, "dog"), (3, "hh")))
  }

  "List.hasSubsequence" should "return true when both lists are Nil" in  {
    List.hasSubsequence(Nil, Nil) should be (true)
  }

  "List.hasSubsequence" should "return true when both sublist is Nil" in  {
    List.hasSubsequence(List(9, 14), Nil) should be (true)
  }

  "List.hasSubsequence" should "return true when both Lists are the same" in  {
    List.hasSubsequence(List(9, 14), List(9, 14)) should be (true)
  }

  "List.hasSubsequence" should "return true when sub list is a prefix" in  {
    List.hasSubsequence(List(9, 14, 100), List(9, 14)) should be (true)
  }

  "List.hasSubsequence" should "return true when sub list is a suffix" in  {
    List.hasSubsequence(List(9, 50, 100, 9, 9, 14), List(9, 14)) should be (true)
  }

  "List.hasSubsequence" should "return true when sub list is an infix" in  {
    List.hasSubsequence(List(30, 50, 100, 9, 14, 50, 66, -10, 44), List(9, 14, 50)) should be (true)
  }

  "List.hasSubsequence" should "return true when sub list is an infix and there's a partial match too" in  {
    List.hasSubsequence(List(30, 50, 9, 14, 100, 9, 14, 50, 66, -10, 44), List(9, 14, 50)) should be (true)
  }

  "List.hasSubsequence" should "return false when sub list is not an infix but there's a partial match too" in  {
    List.hasSubsequence(List(30, 50, 9, 14, 100, 99, 14, 50, 66, -10, 44), List(9, 14, 50)) should be (false)
  }
}