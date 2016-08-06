package tv.pluto.icfpGaming

import spire.math.Rational
import tv.pluto.icfp.Parser._
import tv.pluto.icfp._

import scala.collection.mutable
import scala.io.Source
import scala.sys.process._

/**
  * Generates naive solutions of unit squares around centroid of vertices.
  * Reads from problem file and outputs solution files.
  *
  * Created by jchen on 8/5/16.
  */
object ICFPGamingRect {

  def main(args: Array[String]): Unit = {
    val problemFileNames: List[String] = "ls problems".lineStream.filter(_.contains("101")).toList
    val problemCase: String = Source.fromFile("problems/" + problemFileNames.head).getLines().mkString("\n")

    val parsedVertices = parserProblem(problemCase).polygons.head

    val minX = parsedVertices.map(_.x.toDouble).min
    val maxX = parsedVertices.map(_.x.toDouble).max
    val minY = parsedVertices.map(_.y.toDouble).min
    val maxY = parsedVertices.map(_.y.toDouble).max

    def ceil(d: Double, m: Int): Rational = Rational(BigInt(math.ceil(d * m).toLong), BigInt(m))

    val dX = ceil(maxX - minX, 20)
    val dY = ceil(maxY - minY, 20)

    def splits(fraction: Rational) = Stream
      .iterate(0)(_ + 1)
      .takeWhile(i => ((fraction * i) - 1) < fraction)
      .toList

    val points = for {
      i <- splits(dX)
      j <- splits(dY)
      x = i * dX
      y = j * dY
    } yield {
      val xb = if (x >= 1) Rational.one else x
      val yb = if (y >= 1) Rational.one else y

      println(s"point: $xb,$yb")

      (xb, yb)
    }

    val indices: Map[(Rational, Rational), Int] = points.zipWithIndex.toMap

    val edges = (for {
      i <- splits(dX).tail
      j <- splits(dY).tail
      xs = (i - 1) * dX
      ys = (j - 1) * dY
      xe = i * dX
      ye = j * dY
    } yield {
      val xb = if (xe >= 1) Rational.one else xe
      val yb = if (ye >= 1) Rational.one else ye

      // those were not supposed to be edges but polygons
      // this is supposed to generate the facets for the Solution class

      val edges = mutable.ListBuffer[Edge](
        Edge(Point(xs, ys), Point(xs, yb)),
        Edge(Point(xs, ys), Point(xb, ys))
      )

      println(edges)

      if (!(xe equals xb)) // CORNER CASE OF CLIPPED - WRONG - REPLACE WITH - IS LAST CONDITION
        edges += Edge(Point(xb, ys), Point(xb, yb))

      if (!(ye equals yb))
        edges += Edge(Point(xs, yb), Point(xb, yb))

      edges
    }).flatten

    // GENERATE SILLUETHE ASWELL

    Visualizer.visualize(edges)
  }

  def pipeline(filename: String) = {
    val problemCase: String = Source.fromFile("problems/" + filename).getLines().mkString("\n")

    // A list of vertices of the polygon (only one polygon for now,
    // not dealing with polygons with holes)
    val parsedVertices = parserProblem(problemCase).polygons.head
    val center = findCentroid(parsedVertices)
    val square = unitSquareCentroid(center)

    NaiveSolWriter.writeSol(square, filename.split("\\.").head)
  }

  // Naive centroid by summing all points and dividing by num of points
  def findCentroid(points: List[Point]): Point = {
    val numPoints = points.length
    val pointsSum: Point = points.fold(Point(0.0, 0.0)) {
      _.add(_)
    }

    pointsSum.divide(numPoints)
  }

  // Given a centroid point, return a unit square around this point
  // List of four Points from bottom left, ccw
  def unitSquareCentroid(centroid: Point): List[Point] = {
    val x = centroid.x
    val y = centroid.y

    List(
      Point(x - 0.5, y - 0.5),
      Point(x + 0.5, y - 0.5),
      Point(x + 0.5, y + 0.5),
      Point(x - 0.5, y + 0.5)
    )
  }

}
