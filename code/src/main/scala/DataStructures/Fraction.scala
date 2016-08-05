package DataStructures


/**
  * Created by greddy on 8/5/16.
  */


trait Fraction {

  val num:Int
  val den:Int

  override def toString:String = {
    val reduced = this.reduce
    reduced.num + " / " + reduced.den
  }

  def isLegal:Boolean
  def reduce:Fraction
  def signReduce:Fraction
  def gcdReduce:Fraction

  def + (f:Fraction):Fraction
  def - (f:Fraction):Fraction
  def / (f:Fraction):Fraction
  def * (f:Fraction):Fraction

  def + (i:Int):Fraction
  def - (i:Int):Fraction
  def / (i:Int):Fraction
  def * (i:Int):Fraction

  def inverse:Fraction
  def negate:Fraction

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

