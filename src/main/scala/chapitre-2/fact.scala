package org.invisibletech.myfpscala

object fact {
  def factorial(n : Int) : Long = {
    @annotation.tailrec
    def aux(n : Int, acc : Long) : Long = {
      if (n <= 0) acc else aux(n-1, n * acc)
    }

    aux(n, 1)
  } 
}