package cn.edu.nju.websoft.artime.action

import cn.edu.nju.websoft.artime.semantic.constant.TimeUnit.TimeUnit
import cn.edu.nju.websoft.artime.utils.UInt

case class AddToAction(
  v: UInt,
  g: TimeUnit
) extends Action {
  override def getParameterStrings: List[String] = List("Action:AddTo", s"Value:$v", s"TimeUnit:$g")

  override def priority = 0
}
