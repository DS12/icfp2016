package icfp;

import cats.data.Xor
import cats.data.Xor.{Left, Right}

import scala.math.BigInt
import scala.math.sqrt

import spire.math.Rational

case class Point(x: Rational, y: Rational) {
  def -(that: Point): Point = Point(this.x-that.x, this.y-that.y)

  def dot(that: Point): Double = (this.x*that.x + this.y*that.y).doubleValue

  def length: Double = sqrt(this.dot(this))

  def cosineAngleTo(that: Point): Double = this.dot(that) / (sqrt(this.length) * sqrt(that.length))

  override def toString: String = "(" + x.toString + ", " + y.toString + ")"
}

object Point { def apply(coords: Rational*) = new Point(coords(0), coords(1)) }

case class LineSegment(p1: Point, p2: Point){

  override def toString: String = p1.toString + " -> " + p2.toString

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

  // for ax + by + c = 0, returns (a,b,c)
  def stdForm: (Rational, Rational, Rational) =
    slopeIntForm match {
      case None => (1, 0, -p1.x)
      case Some((m,b)) => (-m, 1, -b)
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

  def reflect(ls: LineSegment): LineSegment = ls match {
    case LineSegment(p1, p2) => LineSegment(this.reflect(p1), this.reflect(p2))
  }

  def reflect(ls: Seq[LineSegment]): Seq[LineSegment] = ls.map(this.reflect(_))

  def flip: LineSegment = LineSegment(this.p2, this.p1)


  // http://stackoverflow.com/questions/22668659/calculate-on-which-side-of-a-line-a-point-is
  def leftness(p: Point): Rational = (this.p2.x - this.p1.x)*(p.y - this.p1.y) - (p.x-this.p1.x)*(this.p2.y-this.p1.y)

  // if ls is entirely on the left or right of this, it goes in the left or right list.
  // if ls is intersecting this, split it into two pieces and put one in each.
  def split(ls: LineSegment): (List[LineSegment], List[LineSegment]) = {
    val (l1, l2) = (this.leftness(ls.p1), this.leftness(ls.p2))
    if (l1.isZero && l2.isZero) (Nil, Nil)
    else if (l1 >= 0 && l2 >= 0) (List(ls), Nil)
    else if (l1 <= 0 && l2 <= 0) (Nil, List(ls))
    else {
      val inter = this.intersection(ls)
      inter match {
        case Some(Right(p)) => {
          val i1 = LineSegment(ls.p1, p)
          val i2 = LineSegment(p, ls.p2)
          if (l1 >= 0) (List(i1), List(i2))
          else (List(i2), List(i1))
        }
      }
    }
  }

  def translate(p: Point): LineSegment = LineSegment(p1-p, p2-p)

  // returns two points so that the line segment is, for t in [0, 1], the same as  uv._1 + t * uv._2
  def uv: (Point, Point) = (this.p1, this.p2-this.p1)

  // http://stackoverflow.com/questions/4977491/determining-if-two-line-segments-intersect
  // if they intersect, return something, otherwise nothing.
  // something is either this if the lines are parallel and intersecting or
  // it is a point that they intersect at.
  def intersection(that: LineSegment): Option[LineSegment Xor Point] = {
    val (u0, v0) = this.uv
    val (u1, v1) = that.uv

    val (x00, y00) = (u0.x, u0.y)
    val (x10, y10) = (u1.x, u1.y)
    val (x01, y01) = (v0.x, v0.y)
    val (x11, y11) = (v1.x, v1.y)

    val d = x11 * y01 - x01 * y11

    if (d.isZero) { // lines are parallel!

      if (this.slopeIntForm == that.slopeIntForm) Some(Left(this))
      else {// they must be y-axis-aligned
        if (this.p1.x == that.p1.x) Some(Left(this))
        else None
      }
    } else {
      val s = (1/d)* ((x00 - x10)*y01 - (y00 - y10)*x01)
      val t = (1/d)* -(-(x00 - x10)*y11 + (y00 - y10)*x11)

      if ((s >=0) && (s <= 1) && (t >= 0) && (t <= 1)) {
        (u0, v0) match {
          case (Point(ux, uy), Point(vx, vy)) => Some(Right(Point(ux+vx*t, uy+vy*t)))
        }

      }

      else None
    }
  }


}

case class Polygon(pts: Seq[Point]) {
  def translate(p: Point): Polygon = Polygon(pts.map(_-p))

  def isCCW: Boolean = ???
}

case class Facet(vertices: Seq[Point])


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
  lazy val allPts: Seq[Point] = this.silh.polys.flatMap(_.pts)

  lazy val minX = allPts.map(_.x).reduce(_ min _)
  lazy val minY = allPts.map(_.y).reduce(_ min _)
  lazy val maxX = allPts.map(_.x).reduce(_ max _)
  lazy val maxY = allPts.map(_.y).reduce(_ max _)

  lazy val originPoint: Point = Point(minX, minY)
  lazy val maxPoint: Point = Point(maxX, maxY)

  def translate(p: Point): Problem = Problem(silh.translate(p), skel.translate(p))

  def normalize: Problem = this.translate(originPoint)

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

object IntersectExamples extends App {
  val id1 =
    LineSegment(
      Point(0,0),
      Point(1,1))

  val id2 =
    LineSegment(
      Point(1,1),
      Point(2,2))

  val down1 =
    LineSegment(
      Point(0,1),
      Point(1,0))

  val down2 =
    LineSegment(
      Point(1,1),
      Point(2,0))

  val shitter =
    LineSegment(
      Point(-10,-10),
      Point(-35, -60))

  // s, s, n, n, n
  println(s"${id1.intersection(id2)}")
  println(s"${id1.intersection(down1)}")
  println(s"${down1.intersection(down2)}")
  println(s"${down1.intersection(id2)}")
  println(s"${shitter.intersection(id1)}")
}

object LeftnessExamples extends App {

  val id = LineSegment( Point(0,0), Point(1,1) )
  val p01 = Point(0,1)
  val p10 = Point(1,0)
  val p11 = Point(1,1)
  val pn11 = Point(-1, 1)
  val pn1n1 = Point(-1, -1)
  val p1n1 = Point(1, -1)
  val phh = Point(1/2, 1/2)

  println(s"${id.leftness(p01)}")
  println(s"${id.leftness(p10)}")
  println(s"${id.leftness(phh)}")
  println()

  println(s"${id.flip.leftness(p01)}")
  println(s"${id.flip.leftness(p10)}")
  println(s"${id.flip.leftness(phh)}")
  println()

  val yax = LineSegment(Point(0,0), Point(0,1))
  val xax = LineSegment(Point(0,0), Point(1,0))

  println(s"${yax.leftness(p11)}")
  println(s"${yax.leftness(pn11)}")
  println(s"${yax.flip.leftness(p11)}")
  println(s"${yax.flip.leftness(pn11)}")
  println()

  println(s"${xax.leftness(p11)}")
  println(s"${xax.leftness(p1n1)}")
  println(s"${xax.flip.leftness(p11)}")
  println(s"${xax.flip.leftness(p1n1)}")

}
