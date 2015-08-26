package org.invisibletech.myfpscala

object TryTest {
  def parseInsuranceRateQuote(age : String, numberOfSpeedingTickets: String) : Option[Double] = {
    val optAge : Option[Double] = Try(age.toDouble)
    val optTickets : Option[Double] = Try(numberOfSpeedingTickets.toDouble)

    map2(optAge, optTickets)(insuranceRateQuote)
  }

  def Try[A](a: => A) :Option[A] = {
    try Some(a)
    catch {
      case e: Exception => None
    }
  }

  def lift[A,B](f: A => B): Option[A] => Option[B] = _ map f

  // Got to the point with this one where I had the right sequence of opts 
  // and some kind of maps.  What I struggled with here was how to get
  // rid of a compiler error due to f's return type.  I looked at flatMap but 
  // didn't make the connection until I got to considering using a case statement
  // and decided to look at the author's answer.  Which was using flatMap. Doh!
  def map2[A,B,C](a: Option[A], b: Option[B])(f: (A,B) => C):Option[C] = {
      a flatMap (av => b map (bv => f(av, bv)))
  }

  def insuranceRateQuote(oAge: Double, oTickets: Double) : Double = {
    1000.0 * (25.0/oAge) + 100.0 * oTickets
  }
}