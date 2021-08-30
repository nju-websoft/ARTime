package cn.edu.nju.websoft.artime.semantic.constant

import cn.edu.nju.websoft.artime.semantic.constant.TimeUnit.TimeUnit

object DayOfWeek extends EnumConstants {
  type DayOfWeek = Value
  val Unknown = Value(0)
  val Monday = Value(1)
  val Tuesday = Value(2)
  val Wednesday = Value(3)
  val Thursday = Value(4)
  val Friday = Value(5)
  val Saturday = Value(6)
  val Sunday = Value(7)
  val Weekend = Value(8)

  override val grain: TimeUnit = TimeUnit.Day
  override val upperGrain: TimeUnit = TimeUnit.Week
}
