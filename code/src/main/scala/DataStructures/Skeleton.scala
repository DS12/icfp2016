package DataStructures

/**
  * Created by greddy on 8/5/16.
  */

trait SkeletonT {

  val segList:Seq[EdgeT]
  val numOfEdges = segList.size

  override def toString:String = {

    def makeOneSeg(seg: EdgeT) = seg.toString + "\n"

    val nSeg = segList.length
    val tempSegString = segList.map(makeOneSeg).mkString("")
    val len = tempSegString.length
    nSeg.toString + "\n" + tempSegString.slice(0, len-1)
  }

}

case class Skeleton(segList: List[EdgeT]) extends SkeletonT {}




