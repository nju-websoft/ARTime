package cn.edu.nju.websoft.artime.action

import cn.edu.nju.websoft.artime.semantic.constant.Direction
import cn.edu.nju.websoft.artime.semantic.constant.TimeUnit.TimeUnit

case class ToNextAction(
  override val g: TimeUnit
) extends OffsetAction(1, g, Direction.Future) {
  override def getParameterStrings: List[String] = List("Action:ToNext", s"TimeUnit:$g")
}
