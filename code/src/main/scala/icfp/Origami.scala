package icfp;

import scala.math.BigInt

import spire.math.Rational

case class Point(x: Rational, y: Rational) {
  def -(that: Point): Point = Point(this.x-that.x, this.y-that.y)
}

object Point {
  def apply(coords: Rational*) = new Point(coords(0), coords(1))
}

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

  def translate(p: Point): LineSegment = LineSegment(p1-p, p2-p)

}

case class Polygon(pts: Seq[Point]) {
  def translate(p: Point): Polygon = Polygon(pts.map(_-p))

  def isCCW: Boolean = ???
}

case class Facet(edges: Seq[LineSegment]){
  val vertices: Set[Point] = edges.flatMap(_.endpoints).toSet
}


case class Silhouette(polys: Seq[Polygon]) {
  // the left-most bottom-most point in the silhouette
  def originPoint: Point = {
    def originest(acc: Point, next: Point): Point =
      if (next.x < acc.x) next
      else if ((next.x == acc.x) && (next.y < acc.y)) next
      else acc

    val allPts = polys.flatMap(_.pts)
    allPts.tail.foldLeft(allPts.head)(originest)
  }

  def translate(p: Point): Silhouette = Silhouette(polys.map(_.translate(p)))

  def normalize: Silhouette = this.translate(this.originPoint)

}

case class Skeleton(edges: Seq[LineSegment]) {
  def translate(p: Point): Skeleton = Skeleton(edges map (_.translate(p)))
  val boundary: Set[LineSegment] = Set()
}

case class Problem(silh: Silhouette, skel: Skeleton) {
  def translate(p: Point): Problem = Problem(silh.translate(p), skel.translate(p))

  def normalize: Problem = {
    val orig = silh.originPoint
    val xAligned = this.translate(orig)
    val ys = xAligned.silh.polys.flatMap(poly => poly.pts map (_.y))
    val minY = ys.tail.foldLeft(ys.head)(_ min _)
    xAligned.translate(Point(0,minY))
  }

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

object OrigamiOutThere extends App {

  import OrigamiParse._

  val farAway = 
    """1
5
10000000000000000000000000000000000000000000000000000000000000000000000000,0
10000000000000000000000000000000000000000000000000000000000000000000000001,0
10000000000000000000000000000000000000000000000000000000000000000000000001,1/3
30000000000000000000000000000000000000000000000000000000000000000000000001/3,1
10000000000000000000000000000000000000000000000000000000000000000000000000,1
7
10000000000000000000000000000000000000000000000000000000000000000000000000,0 10000000000000000000000000000000000000000000000000000000000000000000000001,0
10000000000000000000000000000000000000000000000000000000000000000000000000,0 10000000000000000000000000000000000000000000000000000000000000000000000000,1
30000000000000000000000000000000000000000000000000000000000000000000000001/3,1/3 30000000000000000000000000000000000000000000000000000000000000000000000001/3,1
10000000000000000000000000000000000000000000000000000000000000000000000001,0 10000000000000000000000000000000000000000000000000000000000000000000000001,1/3
30000000000000000000000000000000000000000000000000000000000000000000000001/3,1/3 10000000000000000000000000000000000000000000000000000000000000000000000001,1/3
10000000000000000000000000000000000000000000000000000000000000000000000001,1/3 30000000000000000000000000000000000000000000000000000000000000000000000001/3,1
10000000000000000000000000000000000000000000000000000000000000000000000000,1 30000000000000000000000000000000000000000000000000000000000000000000000001/3,1
    """.stripMargin
  val farAway2 = """1
4
-1 -1
1 -1
-1 1
1 1
2
-1 -1 1 1
-1 1 1 -1
"""
  val prob = parseProblem.run(tokenize(farAway2)).value._2
  println(prob.normalize)
}
