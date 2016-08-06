package DataStructures


/**
  * Created by greddy on 8/5/16.
  */


trait FractionT {

  val num:Int
  val den:Int

  override def toString:String = {
    val reduced = this.reduce
    if (reduced.den != 1) reduced.num + "/" + reduced.den
    else reduced.num.toString
  }

  def isLegal:Boolean
  def reduce:FractionT
  def signReduce:FractionT
  def gcdReduce:FractionT

  def + (f:FractionT):FractionT
  def - (f:FractionT):FractionT
  def / (f:FractionT):FractionT
  def * (f:FractionT):FractionT

  def + (i:Int):FractionT
  def - (i:Int):FractionT
  def / (i:Int):FractionT
  def * (i:Int):FractionT

  def sqrt(precision: Int) : FractionT
  def abs : FractionT

  def inverse:FractionT
  def negate:FractionT

  def < (other: FractionT) : Boolean
  def == (other: FractionT) : Boolean
  def <= (other: FractionT) : Boolean

  def gcd:Int = {
    gcd(this.num,this.den)
  }

  def lcd:Int = {
    lcd(this.num, this.den)
  }

  def gcd(x:Int,y:Int):Int = {
    if (y != 0) gcd(y,x%y)
    else x
  }

  def lcd(x:Int,y:Int) = {
    (x * y)/gcd(x,y)
  }

}

