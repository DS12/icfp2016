package tv.pluto.icfpGaming

import spire.math.Rational
import tv.pluto.icfp._
import tv.pluto.icfp.Parser._

import sys.process._
import scala.io.Source

/**
  * Generates naive solutions of unit squares around centroid of vertices.
  * Reads from problem file and outputs solution files.
  *
  * Created by jchen on 8/5/16.
  */
object ICFPGaming {

  def main(args: Array[String]): Unit = {

    val problemFileNames: List[String] = "ls problems".lineStream.take(101).toList

    problemFileNames foreach { fn => println(fn);pipeline(fn) }

/*
    val test = List(Point(1.0, 1.0), Point(2.0, 2.0), Point(3.0, 3.0))
    println("Test case points:")
    test foreach println
    val center = findCentroid(test)
    println(s"Center is $center")
    val square = unitSquareCentroid(center)
    println("Coordinates of square around center:")
    square foreach println

    // --- test from parsing
    val example =
      """
        |1
        |4
        |-1/2,-1/2
        |1/2,-1/2
        |1/2,1/2
        |-1/2,1/2
        |4
        |-1/2,-1/2 1/2,-1/2
        |-1/2,-1/2 -1/2,1/2
        |1/2,-1/2 1/2,1/2
        |-1/2,1/2 1/2,1/2
      """.stripMargin

    // A list of vertices of the polygon (only one polygon for now,
    // not dealing with polygons with holes)
    val parsedEx: List[Point] = parserProblem(example).polygons.head
    println("Example case points:")
    parsedEx foreach println
    val centerEx = findCentroid(parsedEx)
    println(s"Center is $centerEx")
    val squareEx = unitSquareCentroid(centerEx)
    println("Coordinates of square around center:")
    squareEx foreach println

    NaiveSolWriter.writeSol(squareEx, "testOutput")
*/
  }

  def pipeline(filename: String) = {
    val problemCase: String =
      Source.fromFile("problems/" + filename).getLines().mkString("\n")

    // A list of vertices of the polygon (only one polygon for now,
    // not dealing with polygons with holes)
    val parsedVertices = parserProblem(problemCase).polygons.head
    val center = findCentroid(parsedVertices)
    val square = unitSquareCentroid(center)

    NaiveSolWriter.writeSol(square, filename.split("\\.").head)
  }

  // Naive centroid by summing all points and dividing by num of points
  def findCentroid(points: List[Point]): Point = {
    val minX = points.map(_.x).min
    val maxX = points.map(_.x).max
    val minY = points.map(_.y).min
    val maxY = points.map(_.y).max

    val x = (minX + maxX) / 2
    val y = (minY + maxY) / 2

    Point(x, y)
  }

  // Given a centroid point, return a unit square around this point
  // List of four Points from bottom left, ccw
  def unitSquareCentroid(centroid: Point): List[Point] = {
    val x = centroid.x
    val y = centroid.y

    List(Point(x - 0.5, y - 0.5), Point(x + 0.5, y - 0.5), Point(x + 0.5, y + 0.5), Point(x - 0.5, y + 0.5))
  }


}
