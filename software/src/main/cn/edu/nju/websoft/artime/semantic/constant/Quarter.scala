package cn.edu.nju.websoft.artime.semantic.constant

import cn.edu.nju.websoft.artime.semantic.constant.TimeUnit.TimeUnit
import cn.edu.nju.websoft.artime.utils.UInt

case class Quarter (
  value: UInt,
  grain: TimeUnit
)
{
  val unit2Times: Map[TimeUnit.Value, Int] = Map(TimeUnit.HalfY -> 2, TimeUnit.QuarterY -> 1)

  if (!unit2Times.contains(grain)) {
    throw new IllegalArgumentException(s"The grain: $grain is now allowed for a QuarterSlot")
  }

  def getQuarters = {
    value * unit2Times(grain)
  }
}
