package cn.edu.nju.websoft.artime.action

import cn.edu.nju.websoft.artime.semantic.constant.TimeUnit
import cn.edu.nju.websoft.artime.semantic.constant.TimeUnit.TimeUnit
import cn.edu.nju.websoft.artime.utils.UInt

case class ToEndAction(
  override val g: TimeUnit,
  override val up: Option[TimeUnit]
) extends ModifyAction(UInt(-1), g, up) {
  override def getParameterStrings: List[String] =
    if (up.isEmpty || up.get == TimeUnit.Unknown || up.get == TimeUnit.defaultUpperGrain(g)) List("Action:ToEnd", s"TimeUnit:$g")
    else List("Action:ToEnd", s"TimeUnit:$g", s"TimeUnit:${up.get}")
}
