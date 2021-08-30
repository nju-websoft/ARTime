package cn.edu.nju.websoft.artime.semantic

import cn.edu.nju.websoft.artime.action.{Action, RefAction}
import cn.edu.nju.websoft.artime.io.TIMEX3Parser
import cn.edu.nju.websoft.artime.semantic.constant.Direction.Direction
import org.json4s.JsonDSL._
import org.json4s.native.JsonMethods.{compact, render}

case class TimeRef(
  var value: Direction
) extends TimeValue {
  override var isSet: Boolean = false
  override var tid: Int = 0
  override var timex3type: String = "DATE"

  def toTimex3Fmt: String = TIMEX3Parser.direction2string(value)

  def executeAction(a: Action) = {
    a match {
      case a: RefAction => value = a.d
    }
  }

  def toJson =
    ("type" -> "REF") ~
    ("timex3fmt" -> toTimex3Fmt)

  override def toString: String = {
    compact(render(toJson))
  }
}
