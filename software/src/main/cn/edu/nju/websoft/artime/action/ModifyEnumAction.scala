package cn.edu.nju.websoft.artime.action

import cn.edu.nju.websoft.artime.semantic.constant.EnumConstants
import cn.edu.nju.websoft.artime.utils.UInt

case class ModifyEnumAction(
  e: Enumeration#Value,
  t: EnumConstants
) extends ModifyAction(v = UInt(e.id), g = t.grain, up = Option(t.upperGrain)) {

  def this(v: Int, t: EnumConstants) = this(t.apply(v), t)

  override def getParameterStrings: List[String] = List("Action:Modify*", s"Enum:$e")
}
