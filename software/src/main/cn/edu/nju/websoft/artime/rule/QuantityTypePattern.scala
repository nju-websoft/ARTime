package cn.edu.nju.websoft.artime.rule

import cn.edu.nju.websoft.artime.struct.TexToken

case class QuantityTypePattern (
  qType: String
) extends QuantityTokenPattern {
  override def isVariable: Boolean = false
  override def matches(t: TexToken): Boolean = t.qType.contains(qType)
  override def toString: String = s"$qType:_"
}
