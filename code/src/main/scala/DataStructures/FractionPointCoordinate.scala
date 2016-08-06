package DataStructures

/**
  * Created by greddy on 8/5/16.
  */

case class FractionPointCoordinate(num:Int, den:Int) extends FractionT {

  def rr:FractionT = this.reduce

  override def isLegal:Boolean =  den!= 0 && num >= 0 && den >= 0

  def signReduce:FractionT = {
    if (den < 0) this.copy(-num,-den)
    else this
  }

  def gcdReduce: FractionT = {
    val g = gcd(this.num, this.den)
    if (g != 0)this.copy(num = this.num/g, den = this.den/g)
    else this
  }

  override def reduce:FractionT = {
    if (den == 0) this
    else this.gcdReduce.signReduce
  }

  override def negate:FractionT = {
    this.copy(-num, den).reduce
  }

  override def /(f: FractionT): FractionT = {
    if (den == 0 && f.den == 0) return this.copy(num, f.num)
    this.copy(num*f.den, den*f.num).reduce
  }

  override def +(f: FractionT): FractionT = {
    this.copy(num*f.den + den*f.num, den*f.den).reduce
  }

  override def -(f: FractionT): FractionT = {
    FractionPointCoordinate(num * f.den - den*f.num, den*f.den)
  }

  override def *(f: FractionT): FractionT = this.copy(num*f.num, den*f.den).reduce

  override def inverse: FractionT = this.copy(den, num).reduce

  def + (i:Int):FractionT = this + FractionPointCoordinate(i,1)
  def - (i:Int):FractionT = this + (-i)

  override def /(i: Int): FractionT = this / FractionPointCoordinate(i,1) reduce

  override def *(i: Int): FractionT = this * FractionPointCoordinate(i,1) reduce

  override def abs:FractionT = this.copy(-num,den).reduce

  override def > (f:FractionT):Boolean = (this - f).reduce.num > 0
  override def < (f:FractionT):Boolean = (this - f).reduce.num < 0
  override def == (f:FractionT):Boolean = (this - f).reduce.num == 0

  override def > (i:Int):Boolean = false
  override def < (i:Int):Boolean = false
  override def == (i:Int):Boolean = false

}
/*
abstract class InfiniteFraction extends FractionT {

  override def reduce: FractionT = this
  override def signReduce: FractionT = this
  override def gcdReduce: FractionT = this
  override def isLegal: Boolean = false

  override def + (i:Int):FractionT = this
  override def - (i:Int):FractionT = this
  override def / (i:Int):FractionT = this
  override def * (i:Int):FractionT = this

  override def > (i:Int):Boolean = false
  override def < (i:Int):Boolean = false
  override def == (i:Int):Boolean = false

  // TODO Comparisions amongst Infinites
  override def > (f:FractionT):Boolean = false
  override def < (f:FractionT):Boolean = false
  override def == (f:FractionT):Boolean = false

}

object PosInfinitePoint extends InfiniteFraction {


  override val num = 1
  override val den = 0

  override def abs = this

  override def negate: FractionT = NegInfinitePoint

  override def /(f: FractionT): FractionT = f match {
    case PosInfinitePoint => FractionPointCoordinate(1,1)
    case NegInfinitePoint => FractionPointCoordinate(-1,1)
    case _ => this
  }

  override def +(f: FractionT): FractionT = f match {
    case NegInfinitePoint => FractionPointCoordinate(0,1)
    case _ => this
  }

  override def inverse: FractionT = FractionPointCoordinate(0,1)

  override def -(f: FractionT): FractionT = f match {
    case PosInfinitePoint => FractionPointCoordinate(0,1)
    case _ => this
  }

  override def *(f: FractionT): FractionT = f match {
    case NegInfinitePoint => NegInfinitePoint
    case _ => this
  }
}

object NegInfinitePoint extends InfiniteFraction {

  override val num = -1
  override val den = 0

  override def abs = PosInfinitePoint

  override def negate: FractionT = PosInfinitePoint

  override def /(f: FractionT): FractionT = f match {
    case PosInfinitePoint => FractionPointCoordinate(-1,1)
    case NegInfinitePoint => FractionPointCoordinate(1,1)
    case _ => this
  }

  override def +(f: FractionT): FractionT = f match {
    case PosInfinitePoint => FractionPointCoordinate(0,1)
    case _ => this
  }

  override def inverse: FractionT = FractionPointCoordinate(0,1)

  override def -(f: FractionT): FractionT = f match {
    case PosInfinitePoint => FractionPointCoordinate(0,1)
    case _ => this
  }

  override def *(f: FractionT): FractionT = f match {
    case NegInfinitePoint => PosInfinitePoint
    case _ => this
  }
}
*/