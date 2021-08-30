package cn.edu.nju.websoft.artime.utils

case class UInt(
  value: Int
) {
  private var known: Boolean = true
  def isKnown: Boolean = this.known
  def isUnknown: Boolean = !this.known

  def get: Option[Int] = if (known) Option(value) else Option.empty
  def getOr: Either[Char, Int] = if (known) Right(value) else Left('X')

  def unary_- : UInt = if (known) new UInt(-value) else UInt.unknown
  def ==(x: UInt): Boolean = this.known && x.known && this.value == x.value
  def ==(x: Int): Boolean = this.known && this.value == x
  def !=(x: UInt): Boolean = this.known && x.known && this.value != x.value
  def !=(x: Int): Boolean = this.known && this.value != x
  def <(x: UInt): Boolean = this.known && x.known && this.value < x.value
  def <(x: Int): Boolean = this.known && this.value < x
  def <=(x: UInt): Boolean = this.known && x.known && this.value <= x.value
  def <=(x: Int): Boolean = this.known && this.value <= x
  def >(x: UInt): Boolean = this.known && x.known && this.value > x.value
  def >(x: Int): Boolean = this.known && this.value > x
  def >=(x: UInt): Boolean = this.known && x.known && this.value >= x.value
  def >=(x: Int): Boolean = this.known && this.value >= x
  def +(x: UInt): UInt = if (this.known && x.known) new UInt(this.value + x.value) else UInt.unknown
  def +(x: Int): UInt = if (this.known) new UInt(this.value + x) else UInt.unknown
  def -(x: UInt): UInt = if (this.known && x.known) new UInt(this.value - x.value) else UInt.unknown
  def -(x: Int): UInt = if (this.known) new UInt(this.value - x) else UInt.unknown
  def *(x: UInt): UInt = if (this.known && x.known) new UInt(this.value * x.value) else UInt.unknown
  def *(x: Int): UInt = if (this.known) new UInt(this.value * x) else UInt.unknown
  def /(x: UInt): UInt = if (this.known && x.known) new UInt(this.value / x.value) else UInt.unknown
  def /(x: Int): UInt = if (this.known) new UInt(this.value / x) else UInt.unknown
  def %(x: UInt): UInt = if (this.known && x.known) new UInt(this.value % x.value) else UInt.unknown
  def %(x: Int): UInt = if (this.known) new UInt(this.value % x) else UInt.unknown

  override def toString: String = if (known) value.toString else "X"
  def toString(width: Int): String = if (known) s"%0${width}d".format(value) else "X" * width
  def +(x: String): String = this.toString +x
}

object UInt {
  private val unknown = new UInt(0)
  unknown.known = false
  def unknownInt: UInt = unknown
  def apply(value: Int): UInt = new UInt(value)
}
