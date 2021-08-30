package cn.edu.nju.websoft.artime.semantic.constant

import cn.edu.nju.websoft.artime.io.TIMEX3Parser
import cn.edu.nju.websoft.artime.semantic.constant.TimeUnit.TimeUnit

object Season extends EnumConstants {
    type Season = Value
    val Spring = Value(1)
    val Summer = Value(2)
    val Fall = Value(3)
    val Winter = Value(4)
    val Unknown = Value(5)

    def toStringRef(x: Season) = TIMEX3Parser.season2string(x)

    override val grain: TimeUnit = TimeUnit.Season
    override val upperGrain: TimeUnit = TimeUnit.Year
}
