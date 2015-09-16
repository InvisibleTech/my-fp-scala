package org.invisibletech.myfpscala.errorhandling

//hide std library `Option` and `Either`, since we are writing our own in this chapter
import scala.{ Option => _, Either => _, _ }

import scala.util._

sealed trait Either[+E, +A] {
  def map[B](f: A => B): Either[E, B] = this match {
    case Left(e) => Left(e)
    case Right(x) => Right(f(x))
  }

  def flatMap[EE >: E, B](f: A => Either[EE, B]): Either[EE, B] = this match {
    case Left(e) => Left(e)
    case Right(x) => f(x)
  }

  def orElse[EE >: E, B >: A](b: => Either[EE, B]): Either[EE, B] = this match {
    case Left(_) => b
    case Right(x) => Right(x)
  }

  def map2[EE >: E, B, C](b: Either[EE, B])(f: (A, B) => C): Either[EE, C] =
    this flatMap (av => b map (bv => f(av, bv)))
}

case class Left[+E](value: E) extends Either[E, Nothing]
case class Right[+A](value: A) extends Either[Nothing, A]

object Either {
  def _2Int(s: String): Either[String, Int] = {
    Try(s.toInt) match {
      case Success(x) => Right(x)
      case Failure(s) => Left(s.toString)
    }
  }

  def sequence[E, A](es: List[Either[E, A]]): Either[E, List[A]] = {
    es match {
      case Nil => Right(Nil)
      case h :: t => h flatMap (hh => sequence(t) map (hh :: _))
    }
  }

  // Got most of this one based on work on Option, forgot to apply f to head of list.  Ended up comparing to
  // author's.
  def traverse[E, A, B](as: List[A])(f: A => Either[E, B]): Either[E, List[B]] = {
    as.foldRight[Either[E, List[B]]](Right(Nil))((h, t) => f(h).map2(t)(_ :: _))
  }

  def sequenceTrav[E, A](es: List[Either[E, A]]) : Either[E, List[A]] = {
  	traverse(es)(x => x)
  }
}