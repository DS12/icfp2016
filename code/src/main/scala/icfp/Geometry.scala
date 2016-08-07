package icfp

import spire.math.Rational
import scala.math._

object Geometry {

  case class Point(x: Rational, y: Rational) {
    def -(that: Point): Point = Point(this.x - that.x, this.y - that.y)

    def +(that: Point): Point = this - (Point(0,0) - that)

    def dot(that: Point): Double = (this.x * that.x + this.y * that.y).doubleValue

    def length: Double = sqrt(this.dot(this))

    def cosineAngleTo(that: Point): Double = this.dot(that) / (sqrt(this.length) * sqrt(that.length))

    def distanceFrom(that: Point): Double = (this - that).length

    // true if point is at the left hand side of the line, false if point is on or at the right hand side of the line.
    def isAtLeft(line: LineSegment): Boolean = {
      if (line.slope == Double.PositiveInfinity) {
        if (line.p1.y < line.p2.y) x < 0 else x > 0
      }
      else y > line.slope * x
    }

    // find out closest points among otherPoints to this point
    def closestPoints(otherPoints: Seq[Point]): Seq[Point] = {
      val closetDistance = otherPoints.map(p => (p - this).length).min
      otherPoints.filter(p => (p - this).length == closetDistance)
    }
  }

  object Point {
    def apply(coords: Rational*) = new Point(coords(0), coords(1))
  }

  case class ccwPoints(points: Seq[Point]) {
    def sort(input: Seq[Point]): Seq[Point] = {
      val first = points.filter(_.x == points.map(_.x).min).minBy(_.y)
      val second = points.filter(_ != first).map(p => (p, LineSegment(first, p).slope)).maxBy(_._2)._1
      def go(prev: Point, current: Point, left: Seq[Point], sorted: Seq[Point]): Seq[Point] = {
        if (left.isEmpty) current +: sorted
        else {
          val next = current.closestPoints(LineSegment(prev, current).leftMostPoints(left)).head
          go(current, next, left.filter(_ != next), current +: sorted)
        }
      }
      go(first, second, points.filter(p => p != first && p != second), Seq(first))
    }

    lazy val sortedPoints = sort(points)

  }


  case class LineSegment(p1: Point, p2: Point) {
    def ==(that: LineSegment): Boolean =
      (this.p1 == that.p1 && this.p2 == that.p2) || (this.p1 == that.p2 && this.p2 == that.p1)

    def endpoints: Seq[Point] = p1 :: p2 :: Nil

    def reverse: LineSegment = LineSegment(p2, p1)

    // None if line segment is vertical.
    def slopeIntForm: Option[(Rational, Rational)] =
    if ((p2.x - p1.x).isZero) None
    else {
      val slope = (p2.y - p1.y) / (p2.x - p1.x)
      val intercept = p1.y - slope * p1.x
      Some((slope, intercept))
    }

    def slope: Double = slopeIntForm match {
      case Some((slope, _)) => slope.toDouble
      case _ => Double.PositiveInfinity
    }

    // http://stackoverflow.com/questions/3306838/algorithm-for-reflecting-a-point-across-a-line
    def reflect(p: Point): Point = this.slopeIntForm match {
      case None =>
        val commonX = this.p1.x
        val dx = p.x - commonX
        Point(p.x - 2 * dx, p.y)
      case Some((a, c)) =>
        val d = (p.x + (p.y - c) * a) / (1 + a * a)
        Point(2 * d - p.x, 2 * d * a - p.y + 2 * c)
    }

    def reflect(l: LineSegment): LineSegment = LineSegment(this.reflect(l.p1), this.reflect(l.p2))

    def reflect(f: Facet): Facet = Facet(ccwPoints(f.vertices.points.map(this.reflect)))

    def translate(p: Point): LineSegment = LineSegment(p1 - p, p2 - p)

    // compute angle between prev->curr and a curr->frontier
    def TurnAngles(frontier: Point): Double = (p2 - p1).cosineAngleTo(frontier - p1)

    // find out the points making the largest left turn
    def leftMostPoints(frontiers: Seq[Point]): Seq[Point] = {
      val pointsOntheLeft = frontiers.filter(_.isAtLeft(this))
      val leftMostAngle = {
        if (pointsOntheLeft.isEmpty) frontiers.map(p => TurnAngles(p)).max
        else pointsOntheLeft.map(p => TurnAngles(p)).min
      }
      frontiers.filter(TurnAngles(_) == leftMostAngle)
    }
  }

  //
  def nextEdge(currEdge: LineSegment, allEdges: Seq[LineSegment]): LineSegment = {
    val frontiers = edgeToVertex(allEdges.filter(_.endpoints.contains(currEdge.p2))).filter(p => p != currEdge.p1 && p != currEdge.p2)
    val goodPoint = currEdge.p2.closestPoints(currEdge.leftMostPoints(frontiers)).head
    LineSegment(currEdge.p2, goodPoint)
  }

  // only work for convex polygon
  def genFacet(edge: LineSegment, allEdges: Seq[LineSegment]): Facet = {
    // check if the picture is on the left.
    val directionCorrect = edgeToVertex(allEdges).exists(_.isAtLeft(edge))
    if (!directionCorrect) genFacet(edge.reverse, allEdges)
    else {
      def go(currEdge: LineSegment, acc: Seq[LineSegment]): Seq[LineSegment] = {
        if (acc.contains(currEdge)) acc
        else go(nextEdge(currEdge, allEdges), acc :+ currEdge)
      }
      Facet(ccwPoints(edgeToVertex(go(edge, Seq()))))
    }
  }


  def genBoundary(edges: Seq[LineSegment]): Seq[LineSegment] = {
    val leftMostPoint: Point = edgeToVertex(edges).minBy(_.x)
    val firstEdge: LineSegment = edges.filter(_.endpoints.contains(leftMostPoint)).maxBy(_.slope)
    genFacet(firstEdge, edges).edges
  }


  def edgeToVertex(edges: Seq[LineSegment]): Seq[Point] = {
    edges.flatMap(_.endpoints).distinct
  }

  // require vertices.length >= 2
  def vertexToEdge(vertices: Seq[Point]): Seq[LineSegment] = {
    if (vertices.length == 2) Seq(LineSegment(vertices.head, vertices(1)))
    else {
      val completeGraph = for {
        va <- vertices
        vb <- vertices
        if va != vb
      } yield LineSegment(va, vb)
      genBoundary(completeGraph)
    }
  }
}


object GeometryAnglesExample extends App {

  import Geometry._

  val prev = Point(0, 0)
  val current = Point(0, 1)
  val frontier = List(Point(-1, 2), Point(0, 2), Point(1, 2))
  // println(Geometry.angles(prev, current, frontier))
}
