package DataStructures

/**
  * Created by greddy on 8/5/16.
  */

trait SilhouetteT {

  val polyList: Seq[PolygonT]

  def numOfPolygons:Int = polyList.size


  override def toString = {
    polyList.length + "\n" + polyList.map(_.toString).mkString("\n")
  }

}

case class Silhouette(polyList: Seq[PolygonT])  extends SilhouetteT {}