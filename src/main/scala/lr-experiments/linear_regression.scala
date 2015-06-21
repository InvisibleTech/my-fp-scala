/*
Optimal

mx + b = y

|x = mean of x 
|xy = mean of xy
|x^2 = mean of x squared

m = |x|y - |xy
    ______________
    (|x)^2 - |(x^2)

b = |y - m|x



*/

object LR {
  case class Point(x: Double, y:Double)
  case class Means(mx: Double, my: Double, mxy: Double, mxsq: Double)


  def means(points: List[Point]): Means = {
    val n = points.size
    val foldedT = points.foldLeft((0.0, 0.0, 0.0, 0.0))((a, v) => (a._1+v.x, a._2+v.y, a._3 + (v.x*v.y), a._4 + (v.x * v.x))) 

    Means(foldedT._1/n, foldedT._2/n, foldedT._3/n, foldedT._4/n)
  }

  def optimizedFit(points: List[Point]) : (Double, Double) = {
    val meanz = means(points)
    println(meanz.toString)
    val m = (meanz.mx*meanz.my - meanz.mxy) / (meanz.mx * meanz.mx - meanz.mxsq)
    val b = meanz.my - m*meanz.mx

    (m, b)
   }

   def main(args: Array[String]): Unit = {
     val l = List(Point(-3,  -15), Point(1, 1), Point(5, 17), Point(8, 29), Point(12, 45))
     println("===> " + optimizedFit(l))
   }
   
}