package cn.edu.nju.websoft.artime.semantic.constant

import cn.edu.nju.websoft.artime.semantic.constant.TimeUnit.TimeUnit

trait EnumConstants extends Enumeration {
  val grain: TimeUnit
  val upperGrain: TimeUnit
}
