package org.invisibletech.myfpscala

import org.invisibletech.myfpscala.errorhandling._

object TryTest {
  def parseInsuranceRateQuote(age : String, numberOfSpeedingTickets: String) : Option[Double] = {
    val optAge : Option[Double] = Try(age.toDouble)
    val optTickets : Option[Double] = Try(numberOfSpeedingTickets.toDouble)

    Option.map2(optAge, optTickets)(insuranceRateQuote)
  }

  def Try[A](a: => A) :Option[A] = {
    try Some(a)
    catch {
      case e: Exception => None
    }
  }

  def insuranceRateQuote(oAge: Double, oTickets: Double) : Double = {
    1000.0 * (25.0/oAge) + 100.0 * oTickets
  }
}