package icfp

import cats.data.State
import icfp.Geometry.{LineSegment, Point}
import icfp.OrigamiParse._
import spire.math.Rational
import vizTools.ProblemViewer.GraphicProblemViewer
import vizTools.Problems

/**
  * Created by tianxia on 8/7/16.
  */
case class SimpleFold(problemString: String)(xScale: Double = 1.0, yScale: Double = 1.0) {
  // To approximate all the problems that have some folds.
  // parse in the problem
  val initToks = tokenize(problemString)
  val problem: Problem = parseProblem.run(initToks).value._2

  // experimental: try to rotate the problem and find a smallest "raw area", which means that's the rotation we want.
  case class ProblemPoints(allPts: Seq[Point]) {
    lazy val minX = allPts.map(_.x).min
    lazy val minY = allPts.map(_.y).min
    lazy val maxX = allPts.map(_.x).max
    lazy val maxY = allPts.map(_.y).max
    lazy val width = maxX - minX
    lazy val height = maxY - minY
    lazy val area: Rational = width * height

    lazy val originPoint: Point = Point(minX, minY)
    lazy val maxPoint: Point = Point(maxX, maxY)
    //    def translate(p: Point): Problem = Problem(silh.translate(p), skel.translate(p))
    //
    //    def normalize: Problem = this.translate(originPoint)

  }

  def areaOfRotatedProblems(pp: ProblemPoints)(lines: Seq[LineSegment]): Seq[Rational] = {
    val allRotation: Seq[ProblemPoints] = lines.map((l: LineSegment) => ProblemPoints(pp.allPts.map(l.reflect)))
    allRotation.map((pp: ProblemPoints) => pp.area)
  }

  def pickRotationLine(pp: ProblemPoints)(lines: Seq[LineSegment]): LineSegment = {
    val areas = areaOfRotatedProblems(pp)(lines)
    val minArea = areas.min
    val index = areas.indexWhere((r: Rational) => r == minArea)
    lines(index)
  }

  def pointToString(p: Point): String = {
    val (xn, xd) = (p.x.numerator, p.x.denominator)
    val (yn, yd) = (p.y.numerator, p.y.denominator)
    val stringx = if (xd == 1) s"$xn" else s"$xn/$xd"
    val stringy = if (yd == 1) s"$yn" else s"$yn/$yd"
    s"$stringx,$stringy"
  }

  case class SolutionSource(pp: ProblemPoints) {
    // get the naive area of the problem, and decide grid
    val naiveWidth: Rational = pp.width
    val naiveHeight: Rational = pp.height
    // the number of grids, will equal to the number of folds:
    // if the width(height) is larger than 1, don't fold, otherwise the fold should be the 1/width + 1
    // the additional line is used to adjust the first grid to a scale of the problem area.
    val gridXnum: Int = if (naiveWidth >= 1.0) 1 else math.max(1, (Rational(1) / naiveWidth).toInt) + 1
    val gridYnum: Int = if (naiveHeight >= 1.0) 1 else math.max(1, (Rational(1) / naiveHeight).toInt) + 1
    // indices are used to calculate the source points
    val pointsXindex: Int = gridXnum
    val pointsYindex: Int = gridYnum
    // n and m are 1 indexed, use to calculate the facets
    val n = pointsXindex + 1
    val m = pointsYindex + 1
    val numPoints: Int = n * m

    val xSmartScale = if (naiveWidth >= 1.0) 1.0 else xScale
    val ySmartScale = if (naiveHeight >= 1.0) 1.0 else yScale
    val xValues: Seq[Rational] =
      (0 to pointsXindex).map {
        (i: Int) => i match {
          case 1 => if (naiveWidth >= 1.0) Rational(1) else Rational(naiveWidth * xSmartScale) // the first line is fitting the problem
          case _ => Rational(i.toDouble / pointsXindex)
        }
      }
    val yValues: Seq[Rational] =
      (0 to pointsYindex).map {
        (i: Int) => i match {
          case 1 => if (naiveHeight >= 1.0) Rational(1) else Rational(naiveHeight * ySmartScale) // the first line is fitting the problem
          case _ => Rational(i.toDouble / pointsYindex)
        }
      }

    val sourcePoints: Seq[Point] =
      for {
        y <- yValues
        x <- xValues
      } yield {
        Point(x, y)
      }

    // facets
    val numFacets: Int = (n - 1) * (m - 1)
    val facets: Seq[String] =
      for {
        i <- 1 until n
        j <- 1 until m
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
      x <- 1 to n - 2
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
      y <- 1 to m - 2
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

    val sourceString: String = s"$numPoints \n" +
      (0 until numPoints).map { i: Int => pointToString(sourcePoints(i)) + "\n": String }.reduce(_ + _) +
      s"$numFacets \n" +
      (0 until numFacets).map { i: Int => facets(i): String }.fold("")(_ + _)
  }

  case class SolutionDestination(foldedPoints: Seq[Point])(problem: Problem) {
    // align the left lower conner with the problem
    val referencePoint: Point = Point(
      (foldedPoints.map(_.x).min + foldedPoints.map(_.x).max) / 2.0,
      (foldedPoints.map(_.y).min + foldedPoints.map(_.y).max) / 2.0)
    val problemCenterx2: Point = Point(
      (problem.minX + problem.maxX) / 2.0,
      (problem.minY + problem.maxY) / 2.0)
    val moveToAlign: Point = problemCenterx2 - referencePoint
    val destinationPoints: Seq[Point] = foldedPoints.map(_ + moveToAlign)

    val destinationString: String =
      destinationPoints.indices.map { i: Int => pointToString(destinationPoints(i)) + "\n": String }.reduce(_ + _)
  }

  // pipeline
  // normalize problem
  val normalizedProblem: Problem = problem.normalize
  val normalizedProblemPoints: ProblemPoints = ProblemPoints(normalizedProblem.allPts)
  // generate a bounch of rotation lines and pick one of them
  val pinPoints: Seq[Point] = (-10 to -5).map((i: Int) => Point(i, 10 + i))
  val rotationLines: Seq[LineSegment] = pinPoints.map((p: Point) => LineSegment(Point(0, 0), p))
  val linePicked: LineSegment = pickRotationLine(normalizedProblemPoints)(rotationLines)
  // flip the normalized problem by the line picked
  val flipedProblemPoints: ProblemPoints = ProblemPoints(normalizedProblemPoints.allPts.map(linePicked.reflect))
  // solve the fliped problem
  val solutionSource = SolutionSource(flipedProblemPoints)
  // flip the folded points back
  val flipBackFoldedPoints: Seq[Point] = solutionSource.foldedPoints.map(linePicked.reflect)
  // move the destination to the original problem
  val solutionDestination = SolutionDestination(flipBackFoldedPoints)(problem)

  // Print out the solution
  val solution: String = solutionSource.sourceString + solutionDestination.destinationString
}


object simpleFoldExample extends App {
  val problem = Problems.prob9

  private val solving: String = problem.stripMargin('|')

  GraphicProblemViewer(solving)
  val sF = SimpleFold(solving)()
  println("===")
  println(sF.solution)
}