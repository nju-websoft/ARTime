package cn.edu.nju.websoft.artime.rule

import cn.edu.nju.websoft.artime.struct.TexToken
import org.json4s.JString

trait TokenPattern {
  def isQuantity: Boolean
  def isVariable: Boolean
  def isPlainText: Boolean
  def matches(s: String): Boolean
  def matches(t: TexToken): Boolean
}