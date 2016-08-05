package icfp;

import scala.math.BigInt
case class Rational(n: BigInt, d: BigInt) {
  override def toString: String =
    if (n==0) "0"
    else if (n==d) "1"
    else n.toString + "/" + d.toString

}

case class Point(x: Rational, y: Rational)

object Point { def apply(coords: Rational*) = new Point(coords(0), coords(1)) }

case class LineSegment(p1: Point, p2: Point){
  def ==(that: LineSegment): Boolean = (this.p1 == that.p1 && this.p2 == that.p2) || (this.p1 == that.p2 && this.p2 == that.p1)

  def endpoints: Seq[Point] = p1 :: p2 :: Nil

  def slopeIntForm: (Rational, Rational) = {
    p2._2 - p1._2
  }

  def reflect(p: Point): Point = {

  }
}

case class Polygon(pts: Seq[Point]) {
  def isCCW: Boolean = ???
}

case class Facet(vertices: Seq[Point])

case class Silhouette(polys: Seq[Polygon])
case class Skeleton(edges: Seq[LineSegment])


object Origami {

  type Problem = (Silhouette, Skeleton)

}
