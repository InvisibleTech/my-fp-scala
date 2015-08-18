package org.invisibletech.myfpscala

sealed trait Option[+A] {
  def filter(f: A => Boolean) : Option[A] = this match {
    case Some(x) if (f(x)) => this
    case _ => None 
  }

  def map[B](f: A => B) : Option[B] = this match {
    case Some(x) => Some(f(x))
    case None => None
  }
}

case class Some[+A](get: A) extends Option[A]
case object None extends Option[Nothing]

