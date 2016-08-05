package DataStructures

/**
  * Created by greddy on 8/5/16.
  */

case class FractionPointCoordinate(num:Int, den:Int) extends Fraction {

  require(den != 0 )

  def rr:Fraction = this.reduce

  override def isLegal:Boolean =  den!= 0 && num >= 0 && den >= 0

  def signReduce:Fraction = {
    if (den < 0) this.copy(-num,-den)
    else this
  }

  def gcdReduce: Fraction = {
    val gcd = this.gcd
    this.copy(num = this.num/gcd, den = this.den/gcd)
  }

  override def reduce:Fraction = {
    this.gcdReduce.signReduce
  }

  override def negate:Fraction = {
    this.copy(-num, den)
  }

  override def /(f: Fraction): Fraction = {
    this.copy(num*f.den, den*f.num).reduce
  }

  override def +(f: Fraction): Fraction = {
    this.copy(num*f.den + den*f.num, den*f.den).reduce
  }

  override def -(f: Fraction): Fraction = this + f.negate

  override def *(f: Fraction): Fraction = this.copy(num*f.num, den*f.den).reduce

  override def inverse: Fraction = this.copy(den, num).reduce

  def + (i:Int):Fraction = this + FractionPointCoordinate(i,1)
  def - (i:Int):Fraction = this + (-i)

  override def /(i: Int): Fraction = this * FractionPointCoordinate(i,1)

  override def *(i: Int): Fraction = this / FractionPointCoordinate(1,i).reduce

}

abstract class InfiniteFraction extends Fraction {

  override def reduce: Fraction = this
  override def signReduce: Fraction = this
  override def gcdReduce: Fraction = this
  override def isLegal: Boolean = false

  override def + (i:Int):Fraction = this
  override def - (i:Int):Fraction = this
  override def / (i:Int):Fraction = this
  override def * (i:Int):Fraction = this
}

object PosInfinitePoint extends InfiniteFraction {


  override val num = 1
  override val den = 0

  override def negate: Fraction = NegInfinitePoint

  override def /(f: Fraction): Fraction = f match {
    case PosInfinitePoint => FractionPointCoordinate(1,1)
    case NegInfinitePoint => FractionPointCoordinate(-1,1)
    case _ => this
  }

  override def +(f: Fraction): Fraction = f match {
    case NegInfinitePoint => FractionPointCoordinate(0,1)
    case _ => this
  }

  override def inverse: Fraction = FractionPointCoordinate(0,1)

  override def -(f: Fraction): Fraction = f match {
    case PosInfinitePoint => FractionPointCoordinate(0,1)
    case _ => this
  }

  override def *(f: Fraction): Fraction = f match {
    case NegInfinitePoint => NegInfinitePoint
    case _ => this
  }
}

object NegInfinitePoint extends InfiniteFraction {

  override val num = -1
  override val den = 0

  override def negate: Fraction = PosInfinitePoint

  override def /(f: Fraction): Fraction = f match {
    case PosInfinitePoint => FractionPointCoordinate(-1,1)
    case NegInfinitePoint => FractionPointCoordinate(1,1)
    case _ => this
  }

  override def +(f: Fraction): Fraction = f match {
    case PosInfinitePoint => FractionPointCoordinate(0,1)
    case _ => this
  }

  override def inverse: Fraction = FractionPointCoordinate(0,1)

  override def -(f: Fraction): Fraction = f match {
    case PosInfinitePoint => FractionPointCoordinate(0,1)
    case _ => this
  }

  override def *(f: Fraction): Fraction = f match {
    case NegInfinitePoint => PosInfinitePoint
    case _ => this
  }

}
