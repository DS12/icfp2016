package tv.pluto.icfp

import spire.math.Rational

import scala.util.control.NonFatal

object Parser {

  def parseValue(segment: String): Rational = Rational(segment)

  def parsePoint(segment: String): Point = {
    val parts = segment.split(",")

    Point(parseValue(parts(0)), parseValue(parts(1)))
  }

  def parseEdge(line: String): Edge = {
    val points = line.split(" ")

    Edge(parsePoint(points(0)), parsePoint(points(1)))
  }

  def parserProblem(string: String): Problem = {
    try {
      parserProblemUnsafe(string)
    } catch {
      case NonFatal(e: Throwable) =>
        throw new IllegalArgumentException("File could not be parsed", e)
    }
  }

  private def parserProblemUnsafe(string: String): Problem = {
    var lines = string.split('\n').toList.map(_.trim).filter(_.nonEmpty)
    val nPoly = lines.head.toInt

    lines = lines.tail
    // sorry for the for-loop
    val polygons = for {
      p <- (0 until nPoly.toInt).toList
    } yield {
      val nPoints = lines.head.toInt
      val points = lines.tail.take(nPoints)
      lines = lines.tail.drop(nPoints)

      points.map(parsePoint)
    }

    val edges = lines.tail.map(parseEdge).toSet

    Problem(polygons, edges)
  }

  def main(args: Array[String]) {
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

    println(parserProblem(example))
  }
}

object Point {

  def apply(x: Long, y: Long): Point = Point(BigInt(x), BigInt(y))

  implicit val ordering = Ordering.by[Point, (Rational, Rational)](p => (p.x, p.y))

}

case class Problem(polygons: List[List[Point]], edges: Set[Edge]) {

  private def formatPolygon(points: List[Point]): String =
    s"""
       |${points.size}
       |${points.mkString("\n")}
      """.stripMargin.trim

  override def toString: String =
    s"""
       |${polygons.size}
       |${polygons.map(formatPolygon).mkString("\n")}
       |${edges.size}
       |${edges.mkString("\n")}
     """.stripMargin.trim
}

case class Point(x: Rational, y: Rational) {

  def subtract(otherPoint: Point) = Point(this.x - otherPoint.x, this.y - otherPoint.y)

  def add(otherPoint: Point) = Point(this.x + otherPoint.x, this.y + otherPoint.y)

  def multiply(m: Int) = Point(this.x * m, this.y * m)

  def divide(divisor: Int) = Point(this.x / divisor, this.y / divisor)

  override def toString = s"$x,$y"

}

case class Edge(p1: Point, p2: Point) {

  def center = p1.add(p2) divide 2

  def subtract(p: Point) = Edge(p1 subtract p, p2 subtract p)

  override def toString: String = s"$p1 $p2"

}



