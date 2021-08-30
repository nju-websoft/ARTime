package cn.edu.nju.websoft.artime.rule

trait PlainTexTokenPattern extends TokenPattern{
  override def isQuantity: Boolean = false
  override def isVariable: Boolean = false
  override def isPlainText: Boolean = true
  def pType: String

  override def matches(s: String): Boolean = pType == s
}
