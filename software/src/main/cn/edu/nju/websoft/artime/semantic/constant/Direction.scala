package cn.edu.nju.websoft.artime.semantic.constant

import cn.edu.nju.websoft.artime.io.TIMEX3Parser

object Direction extends Enumeration {
    type Direction  = Value
    val Past = Value(0)
    val Present = Value(1)
    val Future = Value(2)
    val Unknown = Value(3)

    def toStringRef(x: Direction) = TIMEX3Parser.direction2string(x)
  }
