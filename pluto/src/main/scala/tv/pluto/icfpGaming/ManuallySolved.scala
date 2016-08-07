package tv.pluto.icfpGaming

import spire.math.Rational
import tv.pluto.icfp.{Parser, Point}

object ManuallySolved {

  val keyA = Parser.parserProblem(
    """
      |1
      |4
      |0,0
      |1,0
      |1/2,1/2
      |0,1/2
      |5
      |1/2,1/2 1,0
      |0,0 1,0
      |0,0 0,1/2
      |0,1/2 1/2,1/2
      |0,0 1/2,1/2""".stripMargin.trim
  )

  //   val solA =


  def moveSolution(parsedSol: Solution, xTrans: Rational, yTrans: Rational): Solution = {
    val skeleton = parsedSol.skeleton
    val facets = parsedSol.facets
    val silhouette = parsedSol.silhouette

    // Only need to move silhouette
    val newSil = silhouette.map{ aPoint => aPoint.add(Point(xTrans, yTrans)) }

    Solution(skeleton, facets, newSil)
  }
}

