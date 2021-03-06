package org.invisibletech.myfpscala

sealed trait List[+A]
case object Nil extends List[Nothing]
case class Cons[+A](head: A, tail: List[A]) extends List[A]

object List {
  def foldRight[A, B](ls: List[A], z: B)(f: (A, B) => B) : B = ls match {
    case Nil => z
    case Cons(h, t) => f(h, foldRight(t, z)(f))
  }

  def foldLeftViaFoldRight[A,B](ls: List[A], z: B)(f: (B, A) => B) : B = foldRight(reverse(ls), z)((h, acc) => f(acc, h))

  // From the author's site.
  def foldLeftViaFoldRight_NoReverse[A,B](ls: List[A], z:B)(f: (B, A) => B) : B =
    foldRight(ls, (b:B) => b)((a, g) => b => g(f(b, a))) (z)

  def concat[A](lls: List[List[A]]) :List[A] = {
    foldLeft(lls, Nil:List[A])(appendFoldLeft)
  }

  def add1(l : List[Int]) : List[Int] = foldRight(l, Nil:List[Int])((h, t) => Cons(h + 1, t))

  def doubleToString(l: List[Double]) : List[String] = foldRight(l, Nil:List[String])((h, t) => Cons(h.toString, t))

  def map[A,B](l: List[A])(f: A => B): List[B] = foldRight(l, Nil:List[B])((h, t) => Cons(f(h), t))

  def filter[A](ls: List[A])(f: (A) => Boolean) : List[A] = {
    ls match {
      case Nil => Nil
      case Cons(h, t) if f(h) => Cons(h, filter(t)(f))
      case Cons(_, t) => filter(t)(f)
    }
  }

  def flatMap[A,B](l: List[A])(f: A => List[B]): List[B] = {
    concat(map(l)(f))
  }

  def flatFilter[A](ls: List[A])(f: A => Boolean) : List[A] = {
    flatMap(ls)((x: A) => if (f(x)) List(x) else Nil)
  }

  def zipWith[A,B,C](l: List[A], r: List[B])(f: (A, B) => C) : List[C] = {
    (l, r) match {
      case (_, Nil) => Nil
      case (Nil, _) => Nil
      case (Cons(lh, lt), Cons(rh, rt)) => Cons(f(lh, rh), zipWith(lt, rt)(f)) 
    }
  }

  @annotation.tailrec
  def startsWith[A](sup: List[A], sub: List[A]) : Boolean = {
    
    (sup, sub) match {
      case (_, Nil) => true
      case (Cons(supH, supT), Cons(subH, subT)) if (supH == subH) => startsWith(supT, subT)
      case (_, Cons(_, _)) => false
    }
  }

  @annotation.tailrec
  def hasSubsequence[A](sup: List[A], sub: List[A]): Boolean = {
      sup match {
      case Nil => sub == Nil
      case _ if (startsWith(sup, sub)) => true
      case Cons(h, t) => hasSubsequence(t, sub)
    }
  }

  // The goal is a function like this (et: Int) => Cons( 1, Cons(2, Cons(et, Nil)))
  def appendFoldLeft[A](ls: List[A], rs: List[A]): List[A] = {
    foldLeft(ls, (b:List[A]) => b)((g, a) => b => g(Cons(a, b))) (rs) 
  }

  @annotation.tailrec
  def foldLeft[A,B](ls: List[A], z: B)(f: (B, A) => B) : B = ls match {
      case Nil => z
      case Cons(h, t) => foldLeft(t, f(z, h))(f)
  }

  def appendFoldRight[A](ls: List[A], rs: List[A]) : List[A] = {
    foldRight(ls, rs)(Cons(_, _))
  }

  def foldRightViaFoldLeft[A, B](ls: List[A], z: B)(f: (A, B) => B) : B = 
    foldLeft(reverse(ls), z)((acc, h) => f(h, acc))

  // From the author's site.
  def foldRightViaFoldLeft_NoReverse[A,B](ls: List[A], z: B)(f: (A,B) => B): B = 
    foldLeft(ls, (b:B) => b) ((g,a) => b => g(f(a,b))) (z)

  def intListFactory(as: Int*) : List[Int] = {
    // redundant but proving an idea about why foldRight is so generic.
    List.foldRight(List(as: _*), Nil:List[Int])(Cons(_, _))
  }

  def reverse[A](ls: List[A]) : List[A] = List.foldLeft(ls, Nil:List[A])((acc, h) => Cons(h, acc))

  def length[A](l : List[A]) : Int = List.foldRight(l, 0)((_, acc) => acc + 1)

  def sum(ints: List[Int]) : Int = foldRight(ints, 0)(_ + _)

  def product(dubs: List[Double]) : Double = foldRight(dubs, 1.0)(_ * _)

  def apply[A](as: A*): List[A] = if (as.isEmpty) Nil else Cons(as.head, apply(as.tail: _*))

  def tail[A](ls : List[A]) : List[A] =   drop(ls, 1)

  def setHead[A](n: A, ls: List[A]): List[A] = ls match {
    case Nil => Cons(n, Nil)
    case Cons(h, t) => Cons(n, t)
  }

  @annotation.tailrec
  def drop[A](l: List[A], n: Int): List[A] = l match {
    case Nil => Nil
    case _ if n <= 0 => l
    case Cons(_, t) if n > 0 => drop(t, n -1)
  }


  def dropWhile[A](l: List[A])(f: A => Boolean): List[A] = {
    l match {
      case Cons(h, t) if f(h) => dropWhile(t)(f)
      case _ => l
    }
  }

  def init[A](l: List[A]): List[A] = l match {
      case Nil => Nil
      case Cons(h, Nil) => Nil
      case Cons(h, t) => Cons(h, init(t))
  }

}