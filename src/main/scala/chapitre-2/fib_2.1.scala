package org.invisibletech.myfpscala

object fib {
  def fibonacci(n : Int) : Long = {
    def aux(n : Int) : Long = {
      if (n == 0) 0
      else if (n == 1) 1
      else aux(n-1) + aux(n-2)
    }

    aux(n)
  }

  def fibonaccit(n : Int) : Long = {
    @annotation.tailrec
    def aux(n : Int, f0 : Int, f1 : Int) : Long = {
      if (n == 0) f0
      else aux(n-1, f1, f0 + f1)
    }

    aux(n, 0, 1)
  }
}