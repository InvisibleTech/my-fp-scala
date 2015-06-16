package org.invisibletech.myfpscala

object exercise_2_3 {
  def curry[A,B,C](f: (A, B) => C): A => (B => C) = {
    (a: A) => (f(a, _:B))
  }

  def uncurry[A,B,C](f: A => B => C) : (A, B) => C = {
    (a: A, b: B) => (f(a))(b)
  }

  def compose[A,B,C](f: B => C, g: A => B) : A => C = {
    (a: A) => f(g(a))
  }
}