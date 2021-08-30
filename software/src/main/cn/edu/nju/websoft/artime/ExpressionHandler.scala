package cn.edu.nju.websoft.artime

import cn.edu.nju.websoft.artime.action._
import cn.edu.nju.websoft.artime.semantic.constant.TimeUnit.TimeUnit
import cn.edu.nju.websoft.artime.semantic.constant.{Direction, TimeUnit}
import cn.edu.nju.websoft.artime.semantic.{TimeDuration, TimePointImpl, TimeRef, TimeValue}
import cn.edu.nju.websoft.artime.utils.UInt

object ExpressionHandler {

  def getSmallestGrain(actions: IndexedSeq[Action]) = {
    var result = TimeUnit.Forever
    actions.foreach {
      case a: ModifyAction => if (TimeUnit.smallerThan(a.g, result)) result = a.g
      case a: OffsetAction => if (TimeUnit.smallerThan(a.g, result)) result = a.g
      case a: CountAction => if (TimeUnit.smallerThan(a.g.grain, result)) result = a.g.grain
      case a: MakeSetAction => if (TimeUnit.smallerThan(a.g, result)) result = a.g
    }
    result
  }

  def executeMeaning(referTime: Option[TimePointImpl], resultType: String, actions: IndexedSeq[Action]): TimeValue = {
    resultType match {
      case "POINT" =>
        val result = referTime.getOrElse(TimePointImpl.makeUnknown).copy
        actions.foreach(result.executeAction)
        val newSG = getSmallestGrain(actions)
        if (TimeUnit.smallerThan(result.smallestGrain, newSG))
          result.updateSmallestGrain(newSG)
        result
      case "DURATION" =>
        val result = new TimeDuration(Map.empty[TimeUnit, UInt])
        actions.foreach(result.executeAction)
        result
      case "REF" =>
        val result = TimeRef(Direction.Present)
        actions.foreach(result.executeAction)
        result
    }
  }
}
