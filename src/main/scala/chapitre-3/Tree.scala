package org.invisibletech.myfpscala

sealed trait Tree[+A]
case class Leaf[A](value: A) extends Tree[A]
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

object Tree {
  def size[A](tree :Tree[A]): Int = {
    tree match {
      case null => 0
      case Leaf(_) => 1
      case Branch(l, r) => 1 + size(l) + size(r)
    } 
  }

  def max(t :Tree[Int]): Int = {
    t match {
      case Leaf(x) => x
      case Branch(l, r) => max(l) max max(r)
    }
  }
}