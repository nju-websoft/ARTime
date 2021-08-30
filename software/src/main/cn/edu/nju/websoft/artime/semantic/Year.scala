package cn.edu.nju.websoft.artime.semantic

import cn.edu.nju.websoft.artime.semantic.constant.TimeUnit
import cn.edu.nju.websoft.artime.semantic.constant.TimeUnit.TimeUnit
import cn.edu.nju.websoft.artime.utils.UInt

case class Year (
  value: UInt,
  grain: TimeUnit
)
{
  if (!Year.yearGrains.contains(grain)) {
    throw new IllegalArgumentException(s"The grain: $grain is now allowed for a YearSlot")
  }

  def getYearLevelGrainOfDate(grain: TimeUnit): UInt = {
    getYear / Year.yearGrains(grain)
  }

  def getYear: UInt = {
    value * Year.yearGrains(grain)
  }

  def toTimexString: String = {
    value.toString(5 - Year.yearGrains(grain).toString.length)
  }
}

object Year {
  val yearGrains: Map[TimeUnit.Value, UInt] =
    Map(
      TimeUnit.Millennium -> UInt(1000),
      TimeUnit.Century -> UInt(100),
      TimeUnit.Decade -> UInt(10),
      TimeUnit.Year -> UInt(1)
    ).withDefaultValue(UInt.unknownInt)
}
