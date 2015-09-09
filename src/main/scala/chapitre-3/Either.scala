package org.invisibletech.myfpscala.errorhandling


//hide std library `Option` and `Either`, since we are writing our own in this chapter
import scala.{Option => _, Either => _, _}

sealed trait Either[+E, +A] {
	def map[B](f: A => B) : Either[E, B] = this match {
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

object Either  {
	
}