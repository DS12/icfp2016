package DataStructures

/**
  * Created by greddy on 8/5/16.
  */

trait ProblemT {

  val silhouette:SilhouetteT
  val skeleton: SkeletonT

  override def toString = silhouette.toString + "\n" + skeleton.toString
}

case class Problem(silhouette: SilhouetteT, skeleton: SkeletonT) extends ProblemT