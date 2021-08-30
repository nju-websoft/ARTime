package cn.edu.nju.websoft.artime.action

import cn.edu.nju.websoft.artime.semantic.constant.TimeUnit.TimeUnit

case class MakeSetAction(
  g: TimeUnit
) extends Action {
  override def getParameterStrings: List[String] = List("Action:MakeSet", s"TimeUnit:$g")

  override def priority: Int = 0
}
