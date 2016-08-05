package DataStructures

/**
  * Created by greddy on 8/5/16.
  */

case class Slope(value:FractionT)  {

  /**
    * Slope = m = tan(angle) = (num/den)
    * Represented as a ratio of two floats
    */

  override def toString:String =  "Slope with value (m) = " + value.toString

  def isInfinity:Boolean = value match {
    case PosInfinitePoint => true
    case NegInfinitePoint => true
    case _ => false
  }

  def isZero:Boolean = value match {
    case FractionPointCoordinate(0,_) => true
    case _ => false
  }

  /* Will Not use any conversions
  @Deprecated
  def angle:Double = value match {
    case PosInfinitePoint => Math.PI/2.0
    case NegInfinitePoint => - Math.PI/2.0
    case FractionPointCoordinate(n,d) => Math.atan(n/d)
  }
  */

  def inverse:Slope = this.copy(value.inverse)
  def negate:Slope = this.copy(value.negate)

  def -(slope:Slope):Slope = {
    this.copy( (value - slope.value) / ((value * slope.value) + 1).reduce)
  }

}


object main extends App {

  val x = 23
  val f1 = FractionPointCoordinate(90,90)
  val f2 = FractionPointCoordinate(1,4)
  println(f1)
  val s = Slope(f1)
  println(s)

  println("GCD of 23 and 36 is " , f1.gcd(23,46))
  println("LCD of 35 and 42 is ",f1.lcd(35,42) )

  println("diffs is ", f1 - f2)
  println("mult is", f1*f2)
  println("diff is " + s.-(Slope(f2)))


}