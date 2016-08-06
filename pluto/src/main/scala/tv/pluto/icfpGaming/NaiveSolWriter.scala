package tv.pluto.icfpGaming

import java.io._
import tv.pluto.icfp._

/**
  * Created by jchen on 8/5/16.
  */
object NaiveSolWriter {

  def writeSol(destPoints: List[Point], fileName: String) = {

    val fileDest = "./solutions/" + fileName + ".txt"

    val source = List("4", "0,0", "1,0", "1,1", "0,1").mkString("\n") + "\n"
    val facets = List("1", "4 0 1 2 3").mkString("\n") + "\n"
    val destination = destPoints.map{ point => point.toString }.mkString("\n") + "\n"

    val writer = new PrintWriter(new File(fileDest))

    writer.write(source)
    writer.write(facets)
    writer.write(destination)

    writer.close()

  }

}
