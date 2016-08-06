package tv.pluto.icfpGaming

import java.io.{File, PrintWriter}

import spire.math.Rational
import tv.pluto.icfp.{Edge, Point, Visualizer}
import tv.pluto.icfp.ConvexHull.Tupler

import scala.collection.generic.CanBuildFrom
import scala.util.Random

object Problems {

  def generateStripe(stripes: Int): Solution = {
    val lowerTriangular = for {
      i <- 1 to stripes
    } yield {
      val xs = Rational.zero
      val ys = Rational(BigInt(i), BigInt(stripes))

      val xe = ys
      val ye = xs

      Edge(Point(xs, ys), Point(xe, ye))
    }

    val upperTriangular = for {
      i <- 1 until stripes
    } yield {
      val xs = Rational(BigInt(i), BigInt(stripes))
      val ys = Rational.one

      val xe = ys
      val ye = xs

      Edge(Point(xs, ys), Point(xe, ye))
    }

    val lines = lowerTriangular ++ upperTriangular

    val skeleton = List(Point(0, 0)) ++ lines.flatMap(e => List(e.p1, e.p2)) ++ List(Point(1, 1))
    val indices = skeleton.zipWithIndex.toMap

    val facets = {
      val first = Point(0, 0)
      val Edge(second, third) = lines.head

      List(first, second, third).map(x => indices(x))
    } :: {
      val Edge(second, third) = lines.last
      val last = Point(1, 1)

      List(second, third, last).map(x => indices(x))
    } :: lines.tuple.map {
      case (l: Edge, r: Edge) =>
        List(l.p1, l.p2, r.p2, r.p1).map(x => indices(x))
    }.toList

    val off = Rational(1L, stripes)

    val silhouette = (List(Point(0, 0)) ++ lines ++ List(Point(1, 1))).grouped(2).flatMap {
      case List(point: Point, edge@Edge(p1, p2)) if point.x == Rational.zero && point.y == Rational.zero =>
        List(
          (Point(0, 0), indices(point)),
          (p1, indices(p1)),
          (p2, indices(p2))
        )
      case List(e1@Edge(p1x, p1y), e2@Edge(p2x, p2y)) =>
        val c = e1.center
        List(
          (p1x subtract c, indices(p1x)),
          (p1y subtract c, indices(p1y)),
          (p2x subtract c, indices(p2x)),
          (p2y subtract c, indices(p2y))
        )
      case List(point: Point) =>
        List((Point(0, 0), indices(point)))
    }.toList.sortBy(_._2).map(_._1)

    Solution(skeleton, facets, silhouette)
  }

  def main(args: Array[String]) {

    val solutions = for {
      sx <- List(-1, 1)
      sy <- List(-1, 1)
      tx <- List(0, 3)
      i <- 22 to 25
    } yield {
      def projectToHell(point: Point) = point match {
        case Point(x, y) => Point(sx * x + tx, sy * y)
      }

      val sol = generateStripe(i)
      Solution(sol.skeleton, sol.facets, sol.silhouette.map(projectToHell)).toString
    }

    solutions.zipWithIndex.foreach {
      case (content, index) =>
        val writer = new PrintWriter(s"questions/$index.txt")

        writer.println(content)
        writer.close()
    }

  }

}


