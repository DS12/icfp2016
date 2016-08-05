package DataStructures

/**
  * Created by greddy on 8/5/16.
  */

case class FractionPointCoordinate(num:Int, den:Int) extends FractionT {

  require(den != 0 )

  def rr:FractionT = this.reduce

  override def isLegal:Boolean =  den!= 0 && num >= 0 && den >= 0

  def signReduce:FractionT = {
    if (den < 0) this.copy(-num,-den)
    else this
  }

  def gcdReduce: FractionT = {
    val gcd = this.gcd
    this.copy(num = this.num/gcd, den = this.den/gcd)
  }

  override def reduce:FractionT = {
    this.gcdReduce.signReduce
  }

  override def negate:FractionT = {
    this.copy(-num, den)
  }

  override def /(f: FractionT): FractionT = {
    this.copy(num*f.den, den*f.num).reduce
  }

  override def +(f: FractionT): FractionT = {
    this.copy(num*f.den + den*f.num, den*f.den).reduce
  }

  override def -(f: FractionT): FractionT = this + f.negate

  override def *(f: FractionT): FractionT = this.copy(num*f.num, den*f.den).reduce

  override def inverse: FractionT = this.copy(den, num).reduce

  def + (i:Int):FractionT = this + FractionPointCoordinate(i,1)
  def - (i:Int):FractionT = this + (-i)

  override def /(i: Int): FractionT = this * FractionPointCoordinate(i,1)

  override def *(i: Int): FractionT = this / FractionPointCoordinate(1,i).reduce

}

abstract class InfiniteFraction extends FractionT {

  override def reduce: FractionT = this
  override def signReduce: FractionT = this
  override def gcdReduce: FractionT = this
  override def isLegal: Boolean = false

  override def + (i:Int):FractionT = this
  override def - (i:Int):FractionT = this
  override def / (i:Int):FractionT = this
  override def * (i:Int):FractionT = this
}

object PosInfinitePoint extends InfiniteFraction {


  override val num = 1
  override val den = 0

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
