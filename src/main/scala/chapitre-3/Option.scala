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

  def getOrElse[B >: A](default: => B): B = this match {
    case Some(x) => x
    case None => default
  }

  def flatMap[B](f: A => Option[B]) : Option[B] = map(f) getOrElse None
}

case class Some[+A](get: A) extends Option[A]
case object None extends Option[Nothing]

