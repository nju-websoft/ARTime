package cn.edu.nju.websoft.artime.action

import cn.edu.nju.websoft.artime.semantic.constant.Direction.Direction
import cn.edu.nju.websoft.artime.semantic.constant.TimeUnit.TimeUnit

//TODO:   def ToUpcomingAction(g: TimeUnit) = OffsetAction(1, g, Direction.Present)
abstract class OffsetAction(
  val v: Int,
  val g: TimeUnit,
  val d: Direction,
) extends Action {
  override def priority: Int = 2
}