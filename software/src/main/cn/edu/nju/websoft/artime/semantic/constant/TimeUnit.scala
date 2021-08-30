package cn.edu.nju.websoft.artime.semantic.constant

import cn.edu.nju.websoft.artime.semantic.constant

object TimeUnit extends Enumeration {
    type TimeUnit = Value
    val Forever = Value(0)
    val Millennium = Value(1)
    val Century = Value(2)
    val Decade = Value(3)
    val Year = Value(4)
    val HalfY = Value(5)
    val QuarterY = Value(6)
    val Season = Value(7)
    val Month = Value(8)
    val Week = Value(9)
    val Day = Value(10)
    val HalfD = Value(11)
    val POD = Value(12)
    val Hour = Value(13)
    val Minute = Value(14)
    val Second = Value(15)
    val Unknown = Value(16)

    def isDate(timeUnit: TimeUnit) = timeUnit <= Day

    def isTime(timeUnit: TimeUnit) = timeUnit != Unknown && !isDate(timeUnit)

    def next(timeUnit: TimeUnit) = timeUnit match {
      case t if t != Unknown => TimeUnit(timeUnit.id + 1)
    }

    def smallerThan(timeUnit1: TimeUnit, timeUnit2: TimeUnit) = timeUnit1.id > timeUnit2.id
    def smallerOrEqual(timeUnit1: TimeUnit, timeUnit2: TimeUnit) = timeUnit1.id >= timeUnit2.id

    def smaller(timeUnit1: TimeUnit, timeUnit2: TimeUnit) = TimeUnit(Math.max(timeUnit1.id, timeUnit2.id))

  def defaultUpperGrain(g: TimeUnit): TimeUnit = {
    g match {
      case TimeUnit.Millennium => TimeUnit.Forever
      case TimeUnit.Century => TimeUnit.Forever
      case TimeUnit.Decade => TimeUnit.Forever
      case TimeUnit.Year => TimeUnit.Forever
      case TimeUnit.HalfY => TimeUnit.Year
      case TimeUnit.Season => TimeUnit.Year
      case TimeUnit.QuarterY => TimeUnit.Year
      case TimeUnit.Month => TimeUnit.Year
      case TimeUnit.Week => TimeUnit.Year
      case TimeUnit.Day => TimeUnit.Month
      case TimeUnit.HalfD => TimeUnit.Day
      case TimeUnit.POD => TimeUnit.Day
      case TimeUnit.Hour => TimeUnit.Day
      case TimeUnit.Minute => TimeUnit.Hour
      case TimeUnit.Second => TimeUnit.Minute
    }
  }

  def getStringRef(x: TimeUnit): String = x.toString//TIMEX3Parser.grain2String(x)
  }
