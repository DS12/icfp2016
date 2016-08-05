package DataStructures

/**
  * Created by greddy on 8/5/16.
  */

/**
  * An unique edge/ line could be constructed from two points
  * p and q are the two points which define the edge
  * p and q need to be boundaries of the edge
  * p and q are inclusive
  */
trait Edge {

  val p:Point
  val q:Point

}
