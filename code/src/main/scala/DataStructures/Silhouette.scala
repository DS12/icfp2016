package DataStructures

/**
  * Created by greddy on 8/5/16.
  */

trait SilhouetteT {

  val polyList: Seq[PolygonT]

  def numOfPolygons:Int = polyList.size


  override def toString = {

    def makeOnePoly(p: PolygonT) : String = p.toString + "\n"

    val nPoly = polyList.length
    val tempPolyString = polyList.map(p => makeOnePoly(p)).mkString("")
    val len = tempPolyString.length
    nPoly.toString + "\n" + tempPolyString.slice(0, len-1)
  }

}


case class Silhouette(polyList: List[PolygonT])  extends SilhouetteT {}