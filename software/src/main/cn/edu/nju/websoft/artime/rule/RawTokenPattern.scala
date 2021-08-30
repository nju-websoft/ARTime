package cn.edu.nju.websoft.artime.rule

import cn.edu.nju.websoft.artime.struct.TexToken

case class RawTokenPattern(
  value: String
) extends TokenPattern {
  override def isQuantity: Boolean = false

  override def isVariable: Boolean = false

  override def isPlainText: Boolean = false

  override def toString: String = value

  override def matches(s: String): Boolean = value == s

  override def matches(t: TexToken): Boolean = value == t.lemma
}
