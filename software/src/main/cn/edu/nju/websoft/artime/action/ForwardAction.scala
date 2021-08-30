package cn.edu.nju.websoft.artime.action

import cn.edu.nju.websoft.artime.semantic.constant.Direction
import cn.edu.nju.websoft.artime.semantic.constant.TimeUnit.TimeUnit

case class ForwardAction(
  override val v: Int,
  override val g: TimeUnit
) extends OffsetAction(v, g, Direction.Future) {
  override def getParameterStrings: List[String] = List("Action:Forward", s"Value:$v", s"TimeUnit:$g")
}
