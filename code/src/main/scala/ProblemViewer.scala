import icfp.Geometry.Point
import icfp.OrigamiParse._

import scalax.chart.XYChart
import scalax.chart.api._

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
