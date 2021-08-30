package cn.edu.nju.websoft.artime.rule

import cn.edu.nju.websoft.artime.struct.TexToken

case class VariableTokenPattern(
  qType: String,
  varID: Int
) extends QuantityTokenPattern {
  override def isVariable: Boolean = true
  override def matches(t: TexToken): Boolean = t.qType.contains(qType)
  override def toString: String = s"$qType:$$$varID"
}
