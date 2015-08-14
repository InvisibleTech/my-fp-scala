package org.invisibletech.myfpscala

sealed trait Tree[+A]
case class Leaf[A](value: A) extends Tree[A]
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

object Tree {

  def fold[A, B](t :Tree[A])(f: A => B)(g: (B, B) => B): B = {
    t match {
      case Leaf(x) => f(x)
      case Branch(l, r) => g(fold(l)(f)(g), fold(r)(f)(g))
    }
  }

  def size[A](t: Tree[A]): Int = {
    fold(t)(x => 1)(1 + _ + _)
  }

  def max(t :Tree[Int]): Int = {
      fold(t)(x => x)(_ max _)
  } 

  def depth[A](t :Tree[A]): Int = {
    fold(t)(x => 0)((x, y) => 1 + (x max y))
  }

  def map[A, B](t: Tree[A])(f: A => B): Tree[B] ={
    fold(t)((x) => Leaf(f(x)):Tree[B]) ((x, y) => Branch(x, y))
  }

}