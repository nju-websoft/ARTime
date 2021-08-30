package cn.edu.nju.websoft.artime.action

import cn.edu.nju.websoft.artime.semantic.constant.TimeUnit
import cn.edu.nju.websoft.artime.semantic.constant.TimeUnit.TimeUnit
import cn.edu.nju.websoft.artime.utils.UInt

case class ToBeginAction(
  override val g: TimeUnit,
  override val up: Option[TimeUnit]
) extends ModifyAction(UInt(-2), g, up) {
  override def getParameterStrings: List[String] =
    if (up.isEmpty || up.get == TimeUnit.Unknown || up.get == TimeUnit.defaultUpperGrain(g)) List("Action:ToBegin", s"TimeUnit:$g")
    else List("Action:ToBegin", s"TimeUnit:$g", s"TimeUnit:${up.get}")
}
