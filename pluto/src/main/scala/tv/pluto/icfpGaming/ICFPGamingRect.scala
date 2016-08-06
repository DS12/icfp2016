package tv.pluto.icfpGaming

import spire.math.Rational
import tv.pluto.icfp.Parser._
import tv.pluto.icfp._
import tv.pluto.icfp.ConvexHull.Tupler
import java.io._
import scala.io.Source
import scala.sys.process._
import tv.pluto.icfp.Visualizer.plotSolution

/**
  * Generates naive solutions of smallest unrotated rectangle that fits around the silhouette.
  * Reads from problem file and outputs solution files.
  *
  */
object ICFPGamingRect {

  implicit def tuple2ToList[T](t: (T, T)): List[T] = List(t._1, t._2)

  def main(args: Array[String]): Unit = {
//    val problemFileNames: List[String] = "ls problems".lineStream.filter(_.contains("16")).toList
    val problemFileNames: List[String] = "ls problems".lineStream.take(10).toList

    problemFileNames foreach { fn => println(fn); pipeline(fn) }
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
      val shiftedEnd = end.map { case (aPoint: Point, index: Int) => (Point(aPoint.x + shiftBy, aPoint.y), index) }
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
      val shiftedEnd = end.map { case (aPoint: Point, index: Int) => (Point(aPoint.x, aPoint.y + shiftBy), index) }
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

    val indexListLength = indexedVertices.size

    val numIndicesAlongX = splits(dX).length
    val numIndicesAlongY = splits(dY).length

    // --- Generating facets
    val facets: List[List[Int]] = for {
      x <- (0 until numIndicesAlongX - 1).toList.map(_ * numIndicesAlongY)
      y = (x until (x + numIndicesAlongY)).tuple
      yPair <- y
      yPairRevTrans = yPair.reverse.map {
        _ + numIndicesAlongY
      }
    } yield yPair ++ yPairRevTrans

    // --- Generating silhouette

    // Source vertex indices grouped along x or y axis
    val firstXIndexGroup: List[Int] = (0 until numIndicesAlongX).toList.map(_ * numIndicesAlongY)
    val xIndicesGroups: List[List[Int]] = (0 until numIndicesAlongY).toList.map { i => firstXIndexGroup.map(_ + i) }
    val yIndicesGroups: List[List[Int]] = (0 until indexListLength).toList.grouped(numIndicesAlongY).toList

    val xIndexedPointGroups: List[List[(Point, Int)]] =
      xIndicesGroups.map { aGroup: List[Int] =>
        aGroup.map { index: Int => {
          val thePoint: (Rational, Rational) = indexedVertices(index)
          (Point(thePoint._1, thePoint._2), index)
        }
        }
      }

//    println("--- original points:")
//    xIndexedPointGroups.foreach(println(_))

    val shiftedXIndexedPointGroups: List[List[(Point, Int)]] =
      xIndexedPointGroups.map { group: List[(Point, Int)] =>
        translateX(group.take(2), group.drop(2), dX)
      }

//    println("--- original points shifted along x:")
//    shiftedXIndexedPointGroups.foreach(println(_))

    val shiftedXIndexedPoints: List[(Int, Point)] = shiftedXIndexedPointGroups.flatten.map{ x => (x._2, x._1)}
    val shiftedXMap: Map[Int, Point] = shiftedXIndexedPoints.toMap

    val yIndexedPointGroups: List[List[(Point, Int)]] =
      yIndicesGroups.map { aGroup: List[Int] =>
        aGroup.map { index: Int => (shiftedXMap(index), index) }
      }

    val shiftedYIndexedPointGroups: List[List[(Point, Int)]] =
      yIndexedPointGroups.map { group: List[(Point, Int)] =>
        translateY(group.take(2), group.drop(2), dY)
      }

//    println("--- original points shifted along both x and y:")
//    shiftedYIndexedPointGroups.foreach(println(_))

    val shiftedIndexedPoints: List[(Point, Int)] = shiftedYIndexedPointGroups.flatten
    val silhouette: List[Point] =
      shiftedIndexedPoints
        .sortBy(_._2)
        .map(_._1)
        .map { aPoint: Point => Point(aPoint.x + minX, aPoint.y + minY) }

    val skeleton: List[Point] = xIndexedPointGroups.flatten.sortBy(_._2).map(_._1)

    val solved: String = Solution(skeleton, facets, silhouette).toString

//    plotSolution(Solution(skeleton, facets, silhouette))

    val fileDest = "./solutionsRect/" + filename
    val writer = new PrintWriter(new File(fileDest))
    writer.write(solved)
    writer.close()
  }
}
