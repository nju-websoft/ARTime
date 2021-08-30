package cn.edu.nju.websoft.artime.rule

trait QuantityTokenPattern extends TokenPattern {
  override def isQuantity: Boolean = true
  override def isPlainText: Boolean = false
  def qType: String

  override def matches(s: String): Boolean = qType == s
}
