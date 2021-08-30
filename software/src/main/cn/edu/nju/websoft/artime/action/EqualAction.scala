package cn.edu.nju.websoft.artime.action

import cn.edu.nju.websoft.artime.semantic.constant.Direction
import cn.edu.nju.websoft.artime.semantic.constant.TimeUnit.TimeUnit

case class EqualAction(
  override val g: TimeUnit
) extends OffsetAction(0, g, Direction.Present) {
  override def getParameterStrings: List[String] = List("Action:Equal", s"TimeUnit:$g")

  override def priority: Int = 1
}
