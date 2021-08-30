package cn.edu.nju.websoft.artime.action

import cn.edu.nju.websoft.artime.semantic.constant.EnumConstants
import cn.edu.nju.websoft.artime.semantic.constant.TimeUnit.TimeUnit

case class CountAction(
  v: Int,
  e: Enumeration#Value,
  g: EnumConstants,
  up: TimeUnit
) extends Action {
  override def getParameterStrings: List[String] = List("Action:Count", s"Value:$v", s"Enum:$e", s"TimeUnit:$up")

  override def priority: Int = 0
}
