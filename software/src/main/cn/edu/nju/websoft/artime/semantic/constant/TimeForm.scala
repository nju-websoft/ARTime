package cn.edu.nju.websoft.artime.semantic.constant

import cn.edu.nju.websoft.artime.semantic.constant.DateForm.{DateForm, Empty, Y, YD, YH, YM, YMD, YMWU, YQ, YS, YW}
import cn.edu.nju.websoft.artime.semantic.constant.TimeUnit.TimeUnit

object TimeForm extends Enumeration {
    type TimeForm = Value
    val H = Value(0)
    val HM = Value(1)
    val HMS = Value(2)
    val DT = Value(3)
    val Empty = Value(4)

    def hasGrain(tf: TimeForm, g: TimeUnit): Boolean = {
        tf match {
            case H => Set(TimeUnit.HalfD, TimeUnit.Hour) contains g
            case HM => Set(TimeUnit.HalfD, TimeUnit.Hour, TimeUnit.Minute) contains g
            case HMS => Set(TimeUnit.HalfD, TimeUnit.Hour, TimeUnit.Minute, TimeUnit.Second) contains g
            case DT => Set(TimeUnit.HalfD, TimeUnit.POD) contains g
            case Empty => false
        }
    }

}
