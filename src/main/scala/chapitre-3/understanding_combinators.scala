object  understanding_combinators {

// This is snippets I used to break down the answer to exercises on using foldLeft to define
// foldRight without using list reverse.  Getting the answer based on reverse was easy, I had to
// see this solution, break it down like this to get it.  More advanced FP.

val ident : Int => Int = (b:Int) => b
val fun : (Int, Int) => Int = (a, b) => a + b

val combiner : (Int => Int, Int) => Int => Int = (c : Int => Int, a: Int) => (b: Int) => c(fun(a, b))

// So here we see use of these:
val c1 = combiner(ident, 1)
val c2 = combiner(c1, 10)

c2(0)
}
