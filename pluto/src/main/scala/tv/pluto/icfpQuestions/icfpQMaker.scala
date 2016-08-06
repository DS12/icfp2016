package tv.pluto.icfpQuestions

import tv.pluto.icfp._

/**
  * Main class to generate new questions.
  *
  * Created by jchen on 8/5/16.
  */
object icfpQMaker {

  def main(args: Array[String]): Unit = {

    val initSquare =
      Origami(Map("0" -> Point(0,0), "1" -> Point(1,0),
                  "2" -> Point(1,1), "3" -> Point(0,1)),
              List(VertexEdge("0", "1"), VertexEdge("1", "2"),
                VertexEdge("2", "3"), VertexEdge("3", "0")))

  }

}
