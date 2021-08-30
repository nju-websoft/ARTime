package cn.edu.nju.websoft.artime.semantic.constant

import cn.edu.nju.websoft.artime.semantic.constant.TimeUnit.TimeUnit

object TimeOfDay extends EnumConstants {
  type TimeOfDay = Value
  val Unknown = Value(0)
  val Morning = Value(1)
  val MidDay = Value(2)
  val Afternoon = Value(3)
  val Evening = Value(4)
  val Night = Value(5)
  val DayTime = Value(6)
  val PM = Value(7)
  override val grain: TimeUnit = TimeUnit.POD
  override val upperGrain: TimeUnit = TimeUnit.Day
}
