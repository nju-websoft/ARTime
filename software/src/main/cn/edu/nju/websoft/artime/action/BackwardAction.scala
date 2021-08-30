package cn.edu.nju.websoft.artime.action

import cn.edu.nju.websoft.artime.semantic.constant.Direction
import cn.edu.nju.websoft.artime.semantic.constant.TimeUnit.TimeUnit

case class BackwardAction(
  override val v: Int,
  override val g: TimeUnit
) extends OffsetAction(v, g, Direction.Past) {
  override def getParameterStrings: List[String] = List("Action:Backward", s"Value:$v", s"TimeUnit:$g")
}
