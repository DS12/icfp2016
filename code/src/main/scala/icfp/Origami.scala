package icfp

import scala.math.BigInt
case class Rational(n: BigInt, d: BigInt) {
  override def toString: String =
    if (n==0) "0"
    else if (n==d) "1"
    else n.toString + "/" + d.toString
}

case class Point(x: Rational, y: Rational)

object Point { def apply(coords: Rational*) = new Point(coords(0), coords(1)) }

case class Polygon(pts: Seq[Point]) {
  def isCCW: Boolean = ???
}

case class Silhouette(polys: Seq[Polygon])
case class Skeleton(edges: Seq[Origami.LineSegment])

object Origami {

  type LineSegment = (Point, Point)
  type Problem = (Silhouette, Skeleton)

}
