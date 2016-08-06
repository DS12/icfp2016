package tv.pluto.icfpQuestions

import spire.math.Rational
import tv.pluto.icfp.Point

/**
  * Created by jchen on 8/5/16.
  */
object Mover {

  def mirrorPoint(l1: Point, l2:Point, p1: Point): Point ={
    val slope: Rational = (l2.y - l1.y)/(l2.x - l1.x)
    val intercept: Rational = l1.y - slope*l1.x
    val d: Rational = (p1.x + (p1.y - intercept)*slope) / (1+slope*slope)

    val x: Rational = d*2 - p1.x
    val y: Rational = d*2*slope - p1.y + intercept*2

    Point(x,y)
  }

}
