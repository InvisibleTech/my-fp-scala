package org.invisibletech.myfpscala

sealed trait List[+A]
case object Nil extends List[Nothing]
case class Cons[+A](head: A, tail: List[A]) extends List[A]

object List {
  def foldRight[A, B](ls: List[A], z: B)(f: (A, B) => B) : B = ls match {
    case Nil => z
    case Cons(h, t) => f(h, foldRight(t, z)(f))
  }

  def intListFactory(as: Int*) : List[Int] = {
    // redundant but proving an idea about why foldRight is so generic.

    List.foldRight(List(as: _*), Nil.asInstanceOf[List[Int]])((x, z) => Cons(x, z))
  }

  def sum2(ints: List[Int]) : Int = foldRight(ints, 0)(_ + _)

  def prod2(dubs: List[Double]) : Double = foldRight(dubs, 1.0)(_ * _)

  def sum(ints: List[Int]): Int = ints match {
      case Nil => 0
      case Cons(h, t) => h + sum(t)
  }

  def product(doubles: List[Double]): Double = doubles match {
    case Nil => 1.0
    case Cons(h, t) => h * product(t)
  }

  def apply[A](as: A*): List[A] = if (as.isEmpty) Nil else Cons(as.head, apply(as.tail: _*))

  def tail[A](ls : List[A]) : List[A] =   drop(ls, 1)

  def setHead[A](n: A, ls: List[A]): List[A] = ls match {
    case Nil => Cons(n, Nil)
    case Cons(h, t) => Cons(n, t)
  }

  @annotation.tailrec
  def drop[A](l: List[A], n: Int): List[A] = l match {
    case Nil => Nil
    case _ if n == 0 => l
    case Cons(h, t) if n > 0 => drop(t, n -1)
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