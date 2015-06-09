package org.invisibletech.myfpscala

object MyModule {
  def abs(x : Int) : Long = {
    if (x < 0) -x else x 
  }

  def factorial(n : Int) : Long = {
    def aux(n : Int, acc : Long) : Long = {
      if (n <= 0) acc
      else aux(n-1, n * acc)
    }

    aux(n, 1)
  }

  def formatFn(fnn : String, fn : Int => Long ,x: Int) : String = {
    s"The $fnn of ($x) is (${fn(x)})"
  }

  def main(args: Array[String]): Unit = {
    println(formatFn("absolute value", abs, -118))
    println(formatFn("fibonacci number", fib.fibonaccit,7))
    println(formatFn("factorial", factorial, 5)) 
   }
}