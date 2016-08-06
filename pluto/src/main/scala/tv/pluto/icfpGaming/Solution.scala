package tv.pluto.icfpGaming

import tv.pluto.icfp.Point

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
