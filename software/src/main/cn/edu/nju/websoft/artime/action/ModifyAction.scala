package cn.edu.nju.websoft.artime.action

import cn.edu.nju.websoft.artime.semantic.constant.TimeUnit.TimeUnit
import cn.edu.nju.websoft.artime.utils.UInt

abstract class ModifyAction(
  val v: UInt,
  val g: TimeUnit,
  val up: Option[TimeUnit]
) extends Action {
  override def priority: Int = 0
}

