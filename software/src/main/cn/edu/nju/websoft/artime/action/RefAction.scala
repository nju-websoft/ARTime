package cn.edu.nju.websoft.artime.action

import cn.edu.nju.websoft.artime.semantic.constant.Direction.Direction

case class RefAction (
  d: Direction
) extends Action {
  override def getParameterStrings: List[String] = List("Action:Ref", s"Direction:$d")

  override def priority: Int = -1
}
