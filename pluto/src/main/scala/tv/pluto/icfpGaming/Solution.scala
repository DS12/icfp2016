package tv.pluto.icfpGaming

import tv.pluto.icfp.{Parser, Point}

object Solution {

  def parse(string: String): Solution = {
    val lines = string.split("\n")
    val skeletonSize = lines.head.toInt

    val (skeleton, facetsSil) = lines.tail.splitAt(skeletonSize)
    val facetCount = facetsSil.head.toInt
    val (facets, silhouette) = facetsSil.tail.splitAt(facetCount)

    val skeletonParsed = skeleton.map(Parser.parsePoint).toList
    val facetsParsed = facets.map(_.split(" ").toList.tail.map(_.toInt)).toList
    val silhouetteParsed = silhouette.map(Parser.parsePoint).toList

    Solution(skeletonParsed, facetsParsed, silhouetteParsed)
  }

  def main(args: Array[String]) {
    val testSolution =
      """4
        |0,0
        |0,1
        |1,0
        |1,1
        |1
        |4 0 1 3 2
        |-6/29,-6/29
        |-6/29,23/29
        |23/29,-6/29
        |23/29,23/29""".stripMargin.trim

    val parsedSolution = Solution.parse(testSolution)
    val printedAgain = parsedSolution.toString
    val parsedAgain = Solution.parse(printedAgain)

    assert(testSolution equals printedAgain, "formated solutions were not the same")
    assert(parsedSolution equals parsedAgain, "parsed solutions were not the same")
  }

}


case class Solution(skeleton: List[Point], facets: List[List[Int]], silhouette: List[Point]) {
  override def toString: String =
    s"""
       |${skeleton.size}
       |${skeleton.mkString("\n")}
       |${facets.size}
       |${facets.map(l => (l.size :: l).mkString(" ")).mkString("\n")}
       |${silhouette.mkString("\n")}
    """.stripMargin.trim
}
