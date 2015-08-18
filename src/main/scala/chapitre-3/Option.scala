package org.invisibletech.myfpscala

sealed trait Option[+A] {
  def filter(f: A => Boolean) : Option[A] = this match {
    case Some(x) if (f(x)) => this
    case _ => None 
  }
}

case class Some[+A](get: A) extends Option[A]
case object None extends Option[Nothing]

