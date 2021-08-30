package cn.edu.nju.websoft.artime.semantic.constant

import cn.edu.nju.websoft.artime.semantic.constant
import cn.edu.nju.websoft.artime.semantic.constant.TimeUnit.TimeUnit

object DateForm extends Enumeration {

    type DateForm = Value
    val Y = Value(0)
    val YH = Value(1)
    val YQ = Value(2)
    val YS = Value(3)
    val YM = Value(4)
    val YMD = Value(5)
    val YMWU = Value(6)
    val YW = Value(7)
    val YWU = Value(8)
    val YD = Value(9)
    val Empty = Value(-1)

    def hasGrain(df: DateForm, g: TimeUnit): Boolean = {
       df match {
           case Y => Set(TimeUnit.Forever, TimeUnit.Millennium, TimeUnit.Century, TimeUnit.Decade, TimeUnit.Year) contains g
           case YH => Set(TimeUnit.Forever, TimeUnit.Millennium, TimeUnit.Century, TimeUnit.Decade, TimeUnit.Year,
               TimeUnit.HalfY) contains g
           case YQ => Set(TimeUnit.Forever, TimeUnit.Millennium, TimeUnit.Century, TimeUnit.Decade, TimeUnit.Year,
               TimeUnit.HalfY, TimeUnit.QuarterY) contains g
           case YS => Set(TimeUnit.Forever, TimeUnit.Millennium, TimeUnit.Century, TimeUnit.Decade, TimeUnit.Year,
               TimeUnit.Season) contains g
           case YM => Set(TimeUnit.Forever, TimeUnit.Millennium, TimeUnit.Century, TimeUnit.Decade, TimeUnit.Year,
               TimeUnit.HalfY, TimeUnit.QuarterY, TimeUnit.Month) contains g
           case YW => Set(TimeUnit.Forever, TimeUnit.Millennium, TimeUnit.Century, TimeUnit.Decade, TimeUnit.Year,
               TimeUnit.Month, TimeUnit.Week) contains g
           case YMD | YMWU | YWU | YD => Set(TimeUnit.Forever, TimeUnit.Millennium, TimeUnit.Century, TimeUnit.Decade, TimeUnit.Year,
               TimeUnit.HalfY, TimeUnit.QuarterY, TimeUnit.Month, TimeUnit.Week, TimeUnit.Day) contains g
           case Empty => false
       }
    }
}
