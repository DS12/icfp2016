package icfp

import cats.data.State
import icfp.Geometry.{LineSegment, Point}
import icfp.OrigamiParse._
import spire.math.Rational
import vizTools.ProblemViewer.GraphicProblemViewer

/**
  * Created by tianxia on 8/7/16.
  */
case class SimpleFold(problemString: String)(xOffset: Int = 0, yOffset: Int = 0) {
  // To approximate all the problems that have some folds.
  // parse in the problem
  val initToks = tokenize(problemString)
  val problem: Problem = parseProblem.run(initToks).value._2

  // get the naive area of the problem, and decide grid
  val naiveWidth: Rational = problem.maxX - problem.minX
  val naiveHeight: Rational = problem.maxY - problem.minY
  // the number of grids, will equal to the number of folds
  val gridXnum: Int = math.max(1, (Rational(1) / naiveWidth).toInt) + xOffset
  val gridYnum: Int = math.max(1, (Rational(1) / naiveHeight).toInt) + yOffset
  // indices are used to calculate the source points
  val pointsXindex: Int = gridXnum
  val pointsYindex: Int = gridYnum
  // n and m are 1 indexed, use to calculate the facets
  val n = pointsXindex + 1
  val m = pointsYindex + 1
  val numPoints: Int = n * m

  val xValues: Seq[Rational] = (0 to pointsXindex).map((i: Int) => Rational(i.toDouble / pointsXindex))
  val yValues: Seq[Rational] = (0 to pointsYindex).map((i: Int) => Rational(i.toDouble / pointsYindex))

  val sourcePoints: Seq[Point] =
    for {
      y <- yValues
      x <- xValues
    } yield {
      Point(x, y)
    }

  def pointToString(p: Point): String = {
    val (xn, xd) = (p.x.numerator, p.x.denominator)
    val (yn, yd) = (p.y.numerator, p.y.denominator)
    val stringx = if (xd == 1) s"$xn" else s"$xn/$xd"
    val stringy = if (yd == 1) s"$yn" else s"$yn/$yd"
    s"$stringx,$stringy"
  }

  // facets
  val numFacets: Int = (n - 1) * (m - 1)
  val facets: Seq[String] =
    for {
      i <- 1 to n - 1
      j <- 1 to m - 1
    } yield {
      val p1: Int = i + (j - 1) * n - 1
      val p2: Int = p1 + 1
      val p3: Int = p2 + n
      val p4: Int = p3 - 1
      s"4 $p1 $p2 $p3 $p4" + "\n"
    }


  // fold the paper to the squre
  // fold the paper to left
  val foldLinesX: Seq[LineSegment] =
  for {
    x <- (1 to n - 2)
  } yield {
    LineSegment(Point(xValues(x), 0), Point(xValues(x), 1))
  }

  def foldLeftOnce(line: LineSegment): State[Seq[Point], Seq[Point]] = State.apply[Seq[Point], Seq[Point]] {
    (points: Seq[Point]) => {
      val newPoints: Seq[Point] = points.indices.map { (i: Int) =>
        if (sourcePoints(i).x > line.p1.x) line.reflect(points(i))
        else points(i)
      }
      (newPoints, newPoints)
    }
  }

  val leftFolds: Seq[State[Seq[Point], Seq[Point]]] = foldLinesX.map((l: LineSegment) => foldLeftOnce(l))

  // fold the paper to down
  val foldLinesY: Seq[LineSegment] =
  for {
    y <- (1 to m - 2)
  } yield {
    LineSegment(Point(0, yValues(y)), Point(1, yValues(y)))
  }

  def foldDownOnce(line: LineSegment): State[Seq[Point], Seq[Point]] = State.apply[Seq[Point], Seq[Point]] {
    (points: Seq[Point]) => {
      val newPoints: Seq[Point] = points.indices.map { (i: Int) =>
        if (sourcePoints(i).y > line.p1.y) line.reflect(points(i))
        else points(i)
      }
      (newPoints, newPoints)
    }
  }

  val downFolds: Seq[State[Seq[Point], Seq[Point]]] = foldLinesY.map((l: LineSegment) => foldDownOnce(l))

  // combine all the folds
  val phantomFold: State[Seq[Point], Seq[Point]] = foldLeftOnce(LineSegment(Point(2, 0), Point(2, 1)))

  val allLeftFolds: State[Seq[Point], Seq[Point]] = leftFolds.fold(phantomFold) {
    (state1: State[Seq[Point], Seq[Point]], state2: State[Seq[Point], Seq[Point]]) =>
      State.apply[Seq[Point], Seq[Point]] {
        (points: Seq[Point]) => {
          val newPoints = state2.run(state1.run(points).value._2).value._2
          (newPoints, newPoints)
        }
      }
  }

  val allFolds: State[Seq[Point], Seq[Point]] = downFolds.fold(allLeftFolds) {
    (state1: State[Seq[Point], Seq[Point]], state2: State[Seq[Point], Seq[Point]]) =>
      State.apply[Seq[Point], Seq[Point]] {
        (points: Seq[Point]) => {
          val newPoints = state2.run(state1.run(points).value._2).value._2
          (newPoints, newPoints)
        }
      }
  }

  val foldedPoints: Seq[Point] = allFolds.run(sourcePoints).value._2

  // align the left lower conner with the problem
  val referencePoint: Point = Point(
    (foldedPoints.map(_.x).min + foldedPoints.map(_.x).max)/2.0,
    (foldedPoints.map(_.y).min + foldedPoints.map(_.y).max)/2.0)
  private val problemCenterx2: Point = Point(
    (problem.minX + problem.maxX)/2.0,
    (problem.minY + problem.maxY)/2.0)
  private val moveToAlign: Point = problemCenterx2 - referencePoint
  val destinationPoints: Seq[Point] = foldedPoints.map(_ + moveToAlign)

  // Print out the solution
  val solution: String = s"$numPoints \n" +
    (0 until numPoints).map { i: Int => pointToString(sourcePoints(i)) + "\n": String }.reduce(_ + _) +
    s"$numFacets \n" +
    (0 until numFacets).map { i: Int => facets(i): String }.fold("")(_ + _) +
    (0 until numPoints).map { i: Int => pointToString(destinationPoints(i)) + "\n": String }.reduce(_ + _)

}


object simpleFoldExample extends App {
  val prob7 =
    """1
      |4
      |4328029871649615121465353437184/8656059743299229793415925725865,-1792728671193156318471947026432/8656059743299229793415925725865
      |10448788414492386111887872752297/8656059743299229793415925725865,4328029871649615121465353437184/8656059743299229793415925725865
      |4328029871649614671950572288681/8656059743299229793415925725865,10448788414492386111887872752297/8656059743299229793415925725865
      |-1792728671193156318471947026432/8656059743299229793415925725865,4328029871649614671950572288681/8656059743299229793415925725865
      |4
      |4328029871649615121465353437184/8656059743299229793415925725865,-1792728671193156318471947026432/8656059743299229793415925725865 10448788414492386111887872752297/8656059743299229793415925725865,4328029871649615121465353437184/8656059743299229793415925725865
      |4328029871649615121465353437184/8656059743299229793415925725865,-1792728671193156318471947026432/8656059743299229793415925725865 -1792728671193156318471947026432/8656059743299229793415925725865,4328029871649614671950572288681/8656059743299229793415925725865
      |-1792728671193156318471947026432/8656059743299229793415925725865,4328029871649614671950572288681/8656059743299229793415925725865 4328029871649614671950572288681/8656059743299229793415925725865,10448788414492386111887872752297/8656059743299229793415925725865
      |10448788414492386111887872752297/8656059743299229793415925725865,4328029871649615121465353437184/8656059743299229793415925725865 4328029871649614671950572288681/8656059743299229793415925725865,10448788414492386111887872752297/8656059743299229793415925725865"""

  val prob8 =
    """1
      |4
      |0,0
      |1/2,0
      |1/2,1/2
      |0,1/2
      |4
      |0,0 1/2,0
      |0,0 0,1/2
      |1/2,0 1/2,1/2
      |0,1/2 1/2,1/2
    """

  private val solving: String = prob8.stripMargin('|')

  GraphicProblemViewer(solving)
  val sF = SimpleFold(solving)(0, 0)
  println(sF.foldedPoints)
  println("===")
  println(sF.solution)
}