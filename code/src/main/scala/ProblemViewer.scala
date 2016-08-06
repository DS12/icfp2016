import icfp.OrigamiParse._
import icfp.Point

import scalax.chart.XYChart
import scalax.chart.api._

object ProblemViewer {

  def toDoublePoint(p: Point): (Double, Double) = (p.x.doubleValue, p.y.doubleValue)

  def problemToGraph(problem: String): XYChart = {

    // parse the problem
    val initToks = tokenize(problem)
    val prob = parseProblem.run(initToks).value._2.normalize

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

    println(problem.length)
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

