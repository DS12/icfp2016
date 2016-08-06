package icfp

import Geometry._


case class Polygon(pts: Seq[Point]) {
  def translate(p: Point): Polygon = Polygon(pts.map(_ - p))

  // go back to this whenever gonna solve for hollow case
  //  def isCCW: Boolean = ???
}

// vertices of a facet should always be ordered ccw
case class Facet(vertices: ccwPoints) {
  require(vertices.pts.length >= 3)

  def edges: Seq[LineSegment] = {
    val shift = vertices.pts.tail :+ vertices.pts.head
    vertices.pts.zip(shift).map(p => LineSegment(p._1, p._2))
  }
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
  def apply(facets: Seq[Facet]): Skeleton = Skeleton(facets.flatMap(_.edges))

  def translate(p: Point): Skeleton = Skeleton(edges map (_.translate(p)))

  val vertices: Seq[Point] = edgeToVertex(edges)
  val boundary: Seq[LineSegment] = genBoundary(edges)
  val boundaryPoints: Seq[Point] = this.boundary.flatMap((segment: LineSegment) => segment.endpoints).distinct
}


case class Problem(silh: Silhouette, skel: Skeleton) {
  lazy val allPts: Seq[Point] = this.silh.polys.flatMap(_.pts)

  lazy val minX = allPts.map(_.x).min
  lazy val minY = allPts.map(_.y).min
  lazy val maxX = allPts.map(_.x).max
  lazy val maxY = allPts.map(_.y).max

  lazy val originPoint: Point = Point(minX, minY)
  lazy val maxPoint: Point = Point(maxX, maxY)

  def translate(p: Point): Problem = Problem(silh.translate(p), skel.translate(p))

  def normalize: Problem = this.translate(originPoint)

}


object OrigamiReflectExample extends App {

  val yAxis = LineSegment(Point(0, 0), Point(0, 1))
  val exPt = Point(1, 0)
  val reflectedExPt = yAxis.reflect(exPt)
  println(s"reflected $exPt over y-axis to get $reflectedExPt")

  val xAxis = LineSegment(Point(0, 0), Point(1, 0))
  val exPt2 = Point(0, 1)
  val reflectedExPt2 = xAxis.reflect(exPt2)
  println(s"reflected $exPt2 over x-axis to get $reflectedExPt2")

  val ident = LineSegment(Point(0, 0), Point(1, 1))
  val ex3 = Point(1.4, 0.1)
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
  val farAway2 =
    """1
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
