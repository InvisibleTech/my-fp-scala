package org.invisibletech.myfpscala

object isOrd_2_2 {
  // To use this you need something like this implicit val f : (Int, Int) => Boolean = (x: Int, y:Int) => x <= y
  def isOrdered[A](array: Array[A])(implicit ordered: (A, A) => Boolean): Boolean = {
    def aux(a: Array[A], i : Int, acc: Boolean) : Boolean = {
      if (i >= a.length - 1) acc
      else aux(a, i+1, acc && ordered(a(i), a(i+1)))
    }

    aux(array, 0, true)
  }
}