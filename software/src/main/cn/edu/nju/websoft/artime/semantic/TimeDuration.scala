package cn.edu.nju.websoft.artime.semantic

import cn.edu.nju.websoft.artime.action.{Action, AddToAction, MakeSetAction}
import cn.edu.nju.websoft.artime.io.TIMEX3Parser
import cn.edu.nju.websoft.artime.semantic.constant.TimeUnit
import cn.edu.nju.websoft.artime.semantic.constant.TimeUnit.TimeUnit
import cn.edu.nju.websoft.artime.utils.UInt
import org.json4s.JsonDSL._
import org.json4s.native.JsonMethods.{compact, render}

import scala.collection.mutable

/**
 */
class TimeDuration(
  val values: mutable.Map[TimeUnit, UInt]
) extends TimeValue {
  var unknown: Boolean = false
  var isSet: Boolean = false
  var tid:Int=0
  var timex3type="DURATION"

  def this(initValues: Seq[(TimeUnit, UInt)]) = {
    this(mutable.Map(initValues: _*))
  }

  def this(initValues: Map[TimeUnit, UInt]) = {
    this(initValues.toSeq)
  }

  def executeAction(a: Action) = {
    a match {
      case a: AddToAction => executeAdd(a.v, a.g)
      case a: MakeSetAction => executeMakeSet(a.g)
    }
  }

  def executeAdd(v: UInt, g: TimeUnit): Unit = {
    if (!unknown) {
      if (g == TimeUnit.Unknown)
        unknown = true
      else {
        g match {
/*
          case TimeUnit.Millennium => values.update(TimeUnit.Year, v * 1000 + values.getOrElse(g, UInt(0)))
          case TimeUnit.Century => values.update(TimeUnit.Year, v * 100 + values.getOrElse(g, UInt(0)))
          case TimeUnit.Decade => values.update(TimeUnit.Year, v * 10 + values.getOrElse(g, UInt(0)))
*/
          case _ => values.update(g, v + values.getOrElse(g, UInt(0)))
        }
      }
    }
  }

  def executeMakeSet(g: TimeUnit): Unit = {
    if (g == TimeUnit.Unknown)
      unknown = true
    if (!unknown)
      values.update(g, UInt.unknownInt)
  }

  def toTimex3Fmt: String = {
    if (unknown || values.isEmpty)
      "PXX"
    else {
      //Wikipedia says that ISO8601 says duration should be PnYnM...nS
      var Y = UInt(0)
      val dStr = TimeUnit.values.toSeq.filter(TimeUnit.isDate).collect {
        case x if values.contains(x) =>
          if (x < TimeUnit.Year) {
            Y += Year.apply(values(x), x).getYear
            ""
          } else if (x == TimeUnit.Year) {
            val year = Y
            Y = UInt(0)
            s"${values(x) + year}${TIMEX3Parser.grain2String(x)}"
          } else {
            if (Y.value != 0) {
              val yStr = s"{${Y.value}}Y"
              Y = UInt(0)
              s"$yStr${values(x)}${TIMEX3Parser.grain2String(x)}"
            } else
              s"${values(x)}${TIMEX3Parser.grain2String(x)}"
          }
      }.mkString("")
      val tStr = TimeUnit.values.toSeq.filter(TimeUnit.isTime).collect {
        case x if values.contains(x) => s"${values(x)}${TIMEX3Parser.grain2String(x)}"
      }.mkString("")
      s"P${if (Y.value != 0) s"${Y.value}Y" else dStr}${if (tStr.nonEmpty) s"T$tStr" else ""}"
    }
  }

  def toJson =
    ("type" -> "DURATION") ~
    ("timex3fmt" -> toTimex3Fmt) ~
    ("isSet" -> isSet)

  override def toString: String = {
    compact(render(toJson))
  }

}

object TimeDuration {
  def makeUnknownDuration: TimeDuration = {
    val du = new TimeDuration(mutable.Map.empty[TimeUnit, UInt])
    du.unknown = true
    du
  }
}