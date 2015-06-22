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

OR

 m = 1/n∑x * 1/n∑y - 1/n∑xy
    ________________________ =
    (1/n∑x)^2 - 1/n∑x^2


    1/n^2∑x∑y - 1/n∑xy
    _________________________ =
    1/n^2(∑x)^2 - 1/n∑x^2


    1/n^2(∑x∑y - n∑xy)
    __________________________ =
    1/n^2((∑x)^2 - n∑x^2)

    (∑x∑y - n∑xy)
    __________________________ 
    ((∑x)^2 - n∑x^2)



*/

object LR {
  case class Point(x: Double, y:Double)
  case class MeanFactors(x: Double, y: Double, xy: Double, xsq: Double)


  def meanFactors(points: List[Point]): MeanFactors = {
    val foldedT = points.foldLeft((0.0, 0.0, 0.0, 0.0))((a, v) => (a._1+v.x, a._2+v.y, a._3 + (v.x*v.y), a._4 + (v.x * v.x))) 

    MeanFactors(foldedT._1, foldedT._2, foldedT._3, foldedT._4)
  }

  def optimizedFit(points: List[Point]) : (Double, Double) = {
    val n = points.size
    val meanFactorz = meanFactors(points)
    println(meanFactorz.toString)
    val m = (meanFactorz.x*meanFactorz.y - n*meanFactorz.xy) / (meanFactorz.x * meanFactorz.x - n*meanFactorz.xsq)
    val b = (1.0/n)*(meanFactorz.y - m*meanFactorz.x)

    (m, b)
   }

   def main(args: Array[String]): Unit = {
     val l = List(Point(-3,  -15), Point(1, 1), Point(5, 17), Point(8, 29), Point(12, 45))
     println("===> " + optimizedFit(l))
   }
   
}