package cn.edu.nju.websoft.artime.semantic

import cn.edu.nju.websoft.artime.semantic.constant.TimeUnit.TimeUnit
import cn.edu.nju.websoft.artime.utils.UInt

trait FuzzyTimePoint extends TimeValue {
  def get(g: TimeUnit, up: TimeUnit): Option[UInt]

  def executeModify(v: UInt, g: TimeUnit, up: TimeUnit)
}
