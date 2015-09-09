package org.invisibletech.myfpscala.errorhandling

import scala.{ Option => _, Either => _, _ }
import scala.{ Some => _}

import scala.util._

object Option {

  def _2Int(s: String) : Option[Int] = {
    Try(s.toInt) match {
      case Success(x) => Some(x)
      case Failure(_) => None
    }
  }

  // Again with some help from author.. mostly I was lazy on this one.
  def traverse[A, B](a: List[A])(f: A => Option[B]): Option[List[B]] = {
    a match {
      case Nil => Some(Nil)
      case h :: t =>  map2(f(h), traverse(t)(f))(_ :: _)
    }
  }

  // This one I did from my own Sequence using foldRight as a model.
  def traverseFoldRight[A,B](a: List[A])(f: A => Option[B]): Option[List[B]] = {
    a.foldRight[Option[List[B]]](Some(List[B]()))((h, acc) => f(h) flatMap (hh => acc map (hh :: _)))
  }

  // Got to the point with this one where I had the right sequence of opts 
  // and some kind of maps.  What I struggled with here was how to get
  // rid of a compiler error due to f's return type.  I looked at flatMap but 
  // didn't make the connection until I got to considering using a case statement
  // and decided to look at the author's answer.  Which was using flatMap. Doh!
  def map2[A, B, C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] = {
    a flatMap (av => b map (bv => f(av, bv)))
  }

  // After dissecting the author's solution for sequence (recursive), I was able to muddle my way to this. Had
  // issues with Scala type inferecing for "z"  value Some(List[A]()), it had to be forced to Option[List[A]].
  // At first I tried using .asInstanceOf which worked.  I saw the author hit the same issue and instead used
  // type specification of foldRight.
  def sequenceByFold[A](a: List[Option[A]]): Option[List[A]] = {
    a.foldRight[Option[List[A]]](Some(List[A]()))((h, acc) => h flatMap (hh => acc map (hh :: _)))
  }

  // I noticed the author's version used map2.  Now look at my code I see why.  map2, so I will try to translate
  // my code above to use it.
  def sequenceByFoldMap2[A](a: List[Option[A]]): Option[List[A]] = {
    a.foldRight[Option[List[A]]](Some(Nil)) (map2(_, _)((x, y) => (x :: y)))
  }

  def sequence[A](a: List[Option[A]]): Option[List[A]] = a match {
    case Nil => Some(Nil)
    case h :: t => h flatMap (hh => sequence(t) map (hh :: _))
  }

  // I stumbled around for a bit to come up with my own, failed solution.  I cut it short and looked at the 
  // author's solution which was not readily transparent to me.
  // 
  // As payment for taking the answer from the teacher, I spent time adding printlns and prior to that
  // reasoning about some simple edge cases.  Hence the block of comments below. 
  // 
  // I want to keep them as they should help others who, like me, don't immediately grok how the None entry 
  // stops recursion.  So when reading this code there's two forms of flow control: the recursion of sequence
  // via List and the control flow between map and the conversion function fed in to flatMap. 
  //
  def sequence_verbose[A](a: List[Option[A]]): Option[List[A]] = a match {
    case Nil => { println("End of List ===>"); Some(Nil) }
    case h :: t => { println(s"recurse h ==> $h"); h flatMap (hh => { println(s"applying map to hh ==> $hh"); sequence(t) map (hh :: _) }) }
  }

  // Figuring out the flow of control for sequence:
  //
  // When called by empty list:
  // case  Nil => Somce(Nil)
  //
  // When called with List(Some(2))
  // case Some(2) :: Nil => Some(2) flatMap (hh => sequence(Nil) map (hh :: _)) --> pending evaluation
  // recursing: case Nil => Some(Nil)
  // return to 
  // case Some(2) :: Nil => Some(2) flatMap (hh =>  [Some(Nil)] map (hh :: _)) --> evaluate flatMap
  //  flatMap -> Some(2).map ((hh => Some(Nil) map (hh :: _))  --> evaluate outter map
  //    map ->  case Some(2) => Some(f(2)) --> evalutae f
  // 
  //        f -> 2 => Some(Nil) map (2 :: _)  --> evaluate inner map
  //          map -> case Some(Nil) => Some((Some(2) :: Nil))  --> returns Some(2 :: Nil) ---> Some(Some(List(2))) 
  //    getOrElse -> Some(Some(List(2))) --> Some(List(2))
  //
  // When called with List(None)
  // case None :: Nil => None flatMap (hh => sequence(Nil) map (hh :: _)) -> pending evaluation, let mapper func be f
  //  flatMap -> None.map (f)
  //    map -> case None => None
  //    getOrElse => None 
  //  Hyp --> f is never applied in this case
  //
  // When called with List(Some(2), None)
  // case Some(2) :: (None :: Nil) => Some(2) flatMap (hh => seq((None :: Nil)) map (hh :: _)) --> evaluating flatmap...
  //  ... from above ... leads to applying the function like so:
  //      (List(2)  ==> [List(None)]) map (List(2) :: _)  ---> again map short circuits and avoids calliing f  returns None
  // rule then is None short circuits recursion process and forces return up chain early with None.
  //

}

sealed trait Option[+A] {
  def filter(f: A => Boolean): Option[A] = this match {
    case Some(x) if (f(x)) => this
    case _ => None
  }

  def map[B](f: A => B): Option[B] = this match {
    case Some(x) => Some(f(x))
    case None => None
  }

  def getOrElse[B >: A](default: => B): B = this match {
    case Some(x) => x
    case None => default
  }

  def flatMap[B](f: A => Option[B]): Option[B] = map(f) getOrElse None

  def orElse[B >: A](ob: => Option[B]): Option[B] = map(Some(_)) getOrElse ob

  def mean(xs: Seq[Double]): Option[Double] = {
    if (!xs.isEmpty) Some(xs.sum / xs.size) else None
  }

  def variance(xs: Seq[Double]): Option[Double] = {
    mean(xs) flatMap (m => mean(xs.map((x: Double) => math.pow(x - m, 2))))
  }

  def lift[A, B](f: A => B): Option[A] => Option[B] = _ map f
}

case class Some[+A](get: A) extends Option[A]
case object None extends Option[Nothing]

