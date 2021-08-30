package cn.edu.nju.websoft.artime.rule

import cn.edu.nju.websoft.artime.struct.TexToken

case class PlainTexTypePattern (pType: String) extends PlainTexTokenPattern
{
  override def isVariable: Boolean = false
  override def matches(t: TexToken): Boolean = t.pType.contains(pType)
  override def toString: String = s"$pType"
}
