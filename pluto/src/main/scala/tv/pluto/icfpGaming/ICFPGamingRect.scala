package tv.pluto.icfpGaming

import java.io._

import spire.math.Rational
import tv.pluto.icfp.ConvexHull.Tupler
import tv.pluto.icfp.Parser._
import tv.pluto.icfp._

import scala.io.Source
import scala.sys.process._

/**
  * Generates naive solutions of smallest unrotated rectangle that fits around the silhouette.
  * Reads from problem file and outputs solution files.
  *
  */
object ICFPGamingRect {

  implicit def tuple2ToList[T](t: (T, T)): List[T] = List(t._1, t._2)

  def main(args: Array[String]): Unit = {
    //    val problemFileNames: List[String] = "ls problems".lineStream.filter(_.contains("1180")).toList
    val problemFileNames: List[String] = "ls problems".lineStream.toList

    problemFileNames foreach {
      fn =>
        try {
          println(fn)
          pipeline(fn)
        } catch {
          case nfe: IllegalArgumentException =>
            System.err.println(s"Could not parse input: $fn" + nfe)
            nfe.printStackTrace()
        }
    }
  }

  def pipeline(filename: String) = {
    val problemCase: String = Source.fromFile("problems/" + filename).getLines().mkString("\n")

    // A list of vertices of the polygon (only one polygon for now,
    // not dealing with polygons with holes)
    val parsedVertices = parserProblem(problemCase).polygons.head

    val minX = parsedVertices.map(_.x).min
    val maxX = parsedVertices.map(_.x).max
    val minY = parsedVertices.map(_.y).min
    val maxY = parsedVertices.map(_.y).max

    def ceil(r: Rational, d: Int) = (r * d).ceil / d

    val dX = ceil(maxX - minX, 20)
    val dY = ceil(maxY - minY, 20)

    def splits(fraction: Rational) = Stream
      .iterate(0)(_ + 1)
      .takeWhile(i => ((fraction * i) - 1) < fraction)
      .toList

    val splitsX: List[Int] = splits(dX)
    val splitsY: List[Int] = splits(dY)

    val labeledPoints = for {
      i <- splitsX
      j <- splitsY
      x = i * dX
      y = j * dY
    } yield {
      val xb = if (x >= 1) Rational.one else x
      val yb = if (y >= 1) Rational.one else y

      //      println(s"point: $xb,$yb")
      ((i, j), Point(xb, yb))
    }

    val skeleton = labeledPoints.map(_._2)

    // All the source vertices, indexed
    val numIndicesAlongX = splitsX.length
    val numIndicesAlongY = splitsY.length

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
    def arrangeSilhouette(i: Int, x: Rational, step: Rational, last: Int) = {
      if (i == last && (x % step) != 0) {
        if (i % 2 == 0) {
          step - (x % step)
        } else {
          x % step
        }
      } else {
        step * (i % 2)
      }
    }
    val silhouette = labeledPoints.map {
      case ((i, j), Point(xb, yb)) =>
        val x = arrangeSilhouette(i, xb, dX, splitsX.last)
        val y = arrangeSilhouette(j, yb, dY, splitsY.last)

        Point(x + minX, y + minY)
    }

    val solved: String = Solution(skeleton, facets, silhouette).toString

    //    println(Solution(skeleton, facets, silhouette))
    //    plotSolution(Solution(skeleton, facets, silhouette))

    val fileDest = "./solutionsRect/" + filename
    val writer = new PrintWriter(new File(fileDest))
    writer.write(solved)
    writer.close()
  }
}
