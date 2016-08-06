package tv.pluto.icfpGaming

import spire.math.Rational
import tv.pluto.icfp.Parser._
import tv.pluto.icfp._
import tv.pluto.icfp.ConvexHull.Tupler
import java.io._
import scala.io.Source
import scala.sys.process._

/**
  * Generates naive solutions of smallest unrotated rectangle that fits around the silhouette.
  * Reads from problem file and outputs solution files.
  *
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

    // All the source vertices, indexed
    val indexedVertices: Map[Int, (Rational, Rational)] = points.zipWithIndex.toMap.map(_.swap)
//    indexedVertices foreach println

    val indexListLength = indexedVertices.size
    val numIndicesOnOneSide = math.sqrt(indexListLength).toInt

    // --- Generating facets
    val facets: List[List[Int]] = for {
        y <- (0 until numIndicesOnOneSide - 1).toList.map(_*numIndicesOnOneSide) // 0, 3
        x = (y until (y + numIndicesOnOneSide)).tuple
        xPair <- x
        xPairRevTrans = xPair.reverse.map{_ + numIndicesOnOneSide}
      } yield (xPair ++ xPairRevTrans).toList
//    facets foreach println


    // --- Generating silhouette

    // Source vertex indices grouped along x or y axis
    val yIndicesGroups: List[List[Int]] = (0 until indexListLength).toList.grouped(numIndicesOnOneSide).toList
    val firstXIndexGroup: List[Int] = (0 until numIndicesOnOneSide).toList.map(_ * numIndicesOnOneSide)
    val xIndicesGroups: List[List[Int]] = (0 until numIndicesOnOneSide).toList.map{ i => firstXIndexGroup.map(_+i) }

    val xIndexedPointGroups: List[List[(Point, Int)]] =
      xIndicesGroups.map{ aGroup: List[Int] =>
        aGroup.map { index: Int => {
          val thePoint: (Rational, Rational) = indexedVertices(index)
          (Point(thePoint._1, thePoint._2), index)
        }}
      }

    println("--- original points:")
    xIndexedPointGroups.foreach(println(_))

    val shiftedXIndexedPointGroups: List[List[(Point, Int)]] =
      xIndexedPointGroups.map { group: List[(Point, Int)] =>
        translateX(group.take(2), group.drop(2), dX)
      }

    println("--- original points shifted along x:")
    shiftedXIndexedPointGroups.foreach(println(_))

    val shiftedXIndexedPoints: List[(Point, Int)] = shiftedXIndexedPointGroups.flatten
    val shiftedXMap: Map[Int, Point] = shiftedXIndexedPoints.toMap.map(_.swap)

    val yIndexedPointGroups: List[List[(Point, Int)]] =
      yIndicesGroups.map { aGroup: List[Int] =>
        aGroup.map { index: Int => (shiftedXMap(index), index) }
      }

    val shiftedYIndexedPointGroups: List[List[(Point, Int)]] =
      yIndexedPointGroups.map { group: List[(Point, Int)] =>
        translateY(group.take(2), group.drop(2), dY)
      }

    println("--- original points shifted along both x and y:")
    shiftedYIndexedPointGroups.foreach(println(_))

    val shiftedIndexedPoints: List[(Point, Int)] = shiftedYIndexedPointGroups.flatten
    val silhouette: List[Point] =
      shiftedIndexedPoints
        .sortBy(_._2)
        .map(_._1)
        .map{ aPoint: Point => Point(aPoint.x + minX, aPoint.y + minY) }

    val skeleton: List[Point] = xIndexedPointGroups.flatten.sortBy(_._2).map(_._1)

    val solved: String = Solution(skeleton, facets, silhouette).toString

    val fileDest = "./solutionsRect/" + problemFileNames.head
    val writer = new PrintWriter(new File(fileDest))
    writer.write(solved)
    writer.close()


/*
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

//      println(edges)

      if (!(xe equals xb)) // CORNER CASE OF CLIPPED - WRONG - REPLACE WITH - IS LAST CONDITION
        edges += Edge(Point(xb, ys), Point(xb, yb))

      if (!(ye equals yb))
        edges += Edge(Point(xs, yb), Point(xb, yb))

      edges
    }).flatten


    // GENERATE SILLUETHE ASWELL

    Visualizer.visualize(edges)
*/
  }

  // Call this function with first two points of group in "front", the rest in "end"
  def translateX(front: List[(Point, Int)], end: List[(Point, Int)], shiftBy: Rational): List[(Point, Int)] = end match {
    case Nil => front
    case x :: Nil => {
      val lastPoint: Point = x._1
      val lastIndex: Int = x._2
      val extraLength: Rational = lastPoint.x - shiftBy
      val shiftedEnd: List[(Point, Int)] = List((Point(shiftBy - extraLength, lastPoint.y), lastIndex))

      front ++ shiftedEnd
    }
    case _ => {
      val shiftedEnd = end.map{ case (aPoint: Point, index: Int) => (Point(aPoint.x + shiftBy, aPoint.y), index) }
      val newFront = front ++ shiftedEnd.take(2)
      val newEnd = shiftedEnd.drop(2)

      translateX(newFront, newEnd, shiftBy)
    }
  }

  // Call this function with first two points of group in "front", the rest in "end"
  def translateY(front: List[(Point, Int)], end: List[(Point, Int)], shiftBy: Rational): List[(Point, Int)] = end match {
    case Nil => front
    case x :: Nil => {
      val lastPoint: Point = x._1
      val lastIndex: Int = x._2
      val extraLength: Rational = lastPoint.y - shiftBy
      val shiftedEnd: List[(Point, Int)] = List((Point(lastPoint.x, shiftBy - extraLength), lastIndex))

      front ++ shiftedEnd
    }
    case _ => {
      val shiftedEnd = end.map{ case (aPoint: Point, index: Int) => (Point(aPoint.x, aPoint.y + shiftBy), index) }
      val newFront = front ++ shiftedEnd.take(2)
      val newEnd = shiftedEnd.drop(2)

      translateY(newFront, newEnd, shiftBy)
    }
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
