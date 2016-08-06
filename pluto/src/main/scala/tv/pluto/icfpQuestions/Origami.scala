package tv.pluto.icfpQuestions

import tv.pluto.icfp._

/**
  * Created by jchen on 8/5/16.
  */
case class Origami(vertexMap: Map[String, Point], edgeList: List[VertexEdge]) {

  // Given two points that are known to be on two edges,
  // fold along the edge between these two points
  def simpleFold(pointX: Point, edgeX: VertexEdge, pointY: Point, edgeY: VertexEdge) = {

  }

  def lineEqn(pointX: Point, pointY: Point) = {

  }

  def randomPoints(vm : Map[String, Point], el: List[VertexEdge]): (Point,VertexEdge,Point,VertexEdge) = {
    ???
  }


}

case class VertexEdge(pointA: String, pointB: String)