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

  def orElse[B >: A](ob: => Option[B]) : Option[B] = map(Some(_)) getOrElse ob

  def mean(xs: Seq[Double]) : Option[Double] = {
    if (!xs.isEmpty) Some(xs.sum/xs.size) else None
  }

  def variance(xs: Seq[Double]) : Option[Double] ={
    mean(xs) flatMap (m => mean(xs.map((x:Double) => math.pow(x - m , 2))))
  }
}

case class Some[+A](get: A) extends Option[A]
case object None extends Option[Nothing]

