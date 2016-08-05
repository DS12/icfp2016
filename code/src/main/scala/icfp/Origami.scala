package icfp;

import scala.math.BigInt

import spire.math.Rational

case class Point(x: Rational, y: Rational)

object Point { def apply(coords: Rational*) = new Point(coords(0), coords(1)) }

case class LineSegment(p1: Point, p2: Point){
  def ==(that: LineSegment): Boolean = (this.p1 == that.p1 && this.p2 == that.p2) || (this.p1 == that.p2 && this.p2 == that.p1)

  def endpoints: Seq[Point] = p1 :: p2 :: Nil

  // None if line segment is vertical.
  def slopeIntForm: Option[(Rational, Rational)] =
    if ((p2.x - p1.x).isZero) None
    else {
      val slope = (p2.y - p1.y) / (p2.x - p1.x)
      val intercept = p1.y - slope*p1.x
      Some((slope, intercept))
    }

  // http://stackoverflow.com/questions/3306838/algorithm-for-reflecting-a-point-across-a-line
  def reflect(p: Point): Point = this.slopeIntForm match {
    case None =>
      {
        val commonX = this.p1.x
        val dx = p.x - commonX
        Point( p.x-2*dx , p.y)
      }
    case Some((a, c)) =>
      {
        val d = (p.x + (p.y - c)*a) / (1 + a*a)
        Point(2*d - p.x, 2*d*a - p.y + 2*c)
      }
  }

}

case class Polygon(pts: Seq[Point]) {
  def isCCW: Boolean = ???
}

case class Facet(vertices: Seq[Point])

case class Silhouette(polys: Seq[Polygon])
case class Skeleton(edges: Seq[LineSegment]){
  val boundary: Set[LineSegment] = ???
}


object Origami {

  type Problem = (Silhouette, Skeleton)

}

object OrigamiReflectExample extends App {

  val yAxis = LineSegment( Point(0, 0), Point(0, 1) )
  val exPt = Point(1, 0)
  val reflectedExPt = yAxis.reflect(exPt)
  println(s"reflected $exPt over y-axis to get $reflectedExPt")

  val xAxis = LineSegment( Point(0,0), Point(1,0) )
  val exPt2 = Point(0, 1)
  val reflectedExPt2 = xAxis.reflect(exPt2)
  println(s"reflected $exPt2 over x-axis to get $reflectedExPt2")

  val ident = LineSegment( Point(0,0), Point(1,1) )
  val ex3 = Point( 1.4, 0.1)
  val rex3 = ident.reflect(ex3)
  println(s"reflected ${ex3.x.doubleValue}, ${ex3.y.doubleValue} over ident to get ${rex3.x.doubleValue}, ${rex3.y.doubleValue}")
}
