package tv.pluto.icfpGaming

import scala.io.Source
import tv.pluto.icfp._
import tv.pluto.icfp.Parser._

import sys.process._
import scala.io.Source
/**
  * Created by kamalgurala on 8/5/16.
  */
object SimilarQuestions {

  def main(args: Array[String]) {
    val problemFileNames: List[String] = "ls problems".lineStream.take(101).toList
    val problems = problemFileNames.map(fn => Source.fromFile("problems/" + fn).getLines().mkString("\n"))
    val la = problems.groupBy(identity).mapValues(_.size).filter(_._2 > 3)
    println(la)

  }

}
