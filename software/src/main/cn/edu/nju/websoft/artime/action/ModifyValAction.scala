package cn.edu.nju.websoft.artime.action

import cn.edu.nju.websoft.artime.semantic.constant.TimeUnit
import cn.edu.nju.websoft.artime.semantic.constant.TimeUnit.TimeUnit
import cn.edu.nju.websoft.artime.utils.UInt

case class ModifyValAction(
  override val v: UInt,
  override val g: TimeUnit,
  override val up: Option[TimeUnit]
) extends ModifyAction(v, g, up) {
  override def getParameterStrings: List[String] =
//    List("Action:BasicModify", s"Value:$v", s"TimeUnit:$g")
    if (up.isEmpty || up.get == TimeUnit.Unknown || up.get == TimeUnit.defaultUpperGrain(g)) List("Action:BasicModify", s"Value:$v", s"TimeUnit:$g")
    else List("Action:BasicModify", s"Value:$v", s"TimeUnit:$g", s"TimeUnit:${up.get}")
}
