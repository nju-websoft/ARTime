package cn.edu.nju.websoft.artime.action

import cn.edu.nju.websoft.artime.semantic.constant.Direction
import cn.edu.nju.websoft.artime.semantic.constant.TimeUnit.TimeUnit

case class ToLastAction(
  override val g: TimeUnit
) extends OffsetAction(1, g, Direction.Past) {
  override def getParameterStrings: List[String] = List("Action:ToLast", s"TimeUnit:$g")
}

