package cn.edu.nju.websoft.artime.semantic.constant

import cn.edu.nju.websoft.artime.semantic.constant.TimeUnit.TimeUnit

object MonthOfYear extends EnumConstants {
  type MonthOfYear = Value
  val Unknown = Value(0)
  val January = Value(1)
  val February = Value(2)
  val March = Value(3)
  val April = Value(4)
  val May = Value(5)
  val June = Value(6)
  val July = Value(7)
  val August = Value(8)
  val September = Value(9)
  val October = Value(10)
  val November = Value(11)
  val December = Value(12)
  override val grain: TimeUnit = TimeUnit.Month
  override val upperGrain: TimeUnit = TimeUnit.Year
}
