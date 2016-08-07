package icfp;

import OrigamiParse._

import scalax.chart.XYChart
import scalax.chart.api._
import org.jfree.ui.RectangleInsets

object ProblemViewer {

  def toDoublePoint(p: Point): (Double, Double) = (p.x.doubleValue, p.y.doubleValue)

  def problemToGraph(problem: String): XYChart = {

    // parse the problem
    val initToks = tokenize(problem)
    val prob = parseProblem.run(initToks).value._2

    // extract the edges
    val edges = prob.skel.edges

    // create data for chart
    val names: Seq[String] = edges.indices.map(_.toString)
    val data: Seq[(String, Seq[(Double, Double)])] = for {
      i <- names.indices
    } yield {
      val name = names(i)
      val series = Seq(toDoublePoint(edges(i).p1), toDoublePoint(edges(i).p2))
      (name, series): (String, Seq[(Double, Double)])
    }

    val chart = XYLineChart(data.toList)
    chart
  }

  def plotLines(
    ls: Seq[LineSegment],
    xm: Double = -0.2,
    xM: Double = 1.2,
    ym: Double = -0.2,
    yM: Double = 1.2
  ): XYChart = {
    val names: Seq[String] = ls.indices.map(_.toString)
    val data: Seq[(String, Seq[(Double, Double)])] =
      names.indices.map { i =>
        (
          names(i),
          Seq(toDoublePoint(ls(i).p1), toDoublePoint(ls(i).p2))
        )
      }

    val p = XYLineChart(data.toList)
    p.plot.setAxisOffset(new RectangleInsets(0.2, 0.2, 0.2, 0.2))
    p
  }
}

object exampleSkeleton extends App {

  val ex =
    """1
4
0,0
1,0
1/2,1/2
0,1/2
5
0,0 1,0
1,0 1/2,1/2
1/2,1/2 0,1/2
0,1/2 0,0
0,0 1/2,1/2
    """

  val chart: XYChart = ProblemViewer.problemToGraph(ex)
  chart.show()
}

object exampleProb8 extends App {

  val ex =
    """1
4
0,0
1/2,0
1/2,1/2
0,1/2
4
0,0 1/2,0
0,0 0,1/2
1/2,0 1/2,1/2
0,1/2 1/2,1/2
    """

  val chart: XYChart = ProblemViewer.problemToGraph(ex)
  chart.show()
}

object exampleProb11 extends App {

  val ex =
    """1
5
0,0
1,0
1,1/3
1/3,1
0,1
7
0,0 1,0
0,0 0,1
1/3,1/3 1/3,1
1,0 1,1/3
1/3,1/3 1,1/3
1,1/3 1/3,1
0,1 1/3,1
    """

  val chart: XYChart = ProblemViewer.problemToGraph(ex)
  chart.show()
}
