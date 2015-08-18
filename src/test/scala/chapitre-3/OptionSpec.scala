package org.invisibletech.myfpscala

import org.scalatest._

class OptionSpec extends FlatSpec with Matchers {
  "filter" should "should return None if given None no matter what filter says" in {
    None.filter(_ => true) shouldBe (None)
  }

  "filter" should "should return None if given Some and filter says false" in {
    val opt = Some(10) 

    Some(10).filter(_ => false) shouldBe (None)
  }

  "filter" should "should return Some if given Some and filter says true" in {
    val opt = Some(10) 

    Some(10).filter(_ => true) shouldBe (Some(10))
  }

  "filter" should "should return Some if given Some and filter passes" in {
    val opt = Some(10) 

    Some(10).filter(_ < 11) shouldBe (Some(10))
  }
}