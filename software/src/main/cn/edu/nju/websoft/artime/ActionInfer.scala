package cn.edu.nju.websoft.artime

import cn.edu.nju.websoft.artime.action._
import cn.edu.nju.websoft.artime.semantic._
import cn.edu.nju.websoft.artime.semantic.constant.TimeUnit.TimeUnit
import cn.edu.nju.websoft.artime.semantic.constant._
import cn.edu.nju.websoft.artime.utils.{Counter, UInt}

import scala.collection.mutable

/**

 * @param refInput
 * @param numConstraints
 * @param tokenNum
 */
class ActionInfer(
  refInput: Option[TimePointImpl],
  val numConstraints: Counter[Int],
  val tokenNum: Int,
  val defaultActionIndex: Map[(TimeUnit, TimeUnit), List[Action]]
) {
  val defaultActions = defaultActionIndex.values.flatten.toList
  val refPoint = refInput.getOrElse(TimePointImpl.makeUnknown)
  val actionLimit = Math.max(3, tokenNum * 2)

  def inferRef(target: TimeRef) = new SolutionSet(RefAction(target.value))

  def checkOffset(v: Int, g: TimeUnit): Array[(Option[Int], List[Action])] = {
    val resultBuilder = mutable.ArrayBuilder.make[(Option[Int], List[Action])]()
    if (v == 1) {
      resultBuilder += ((Option.empty, List(ToNextAction(g))))
    }
    if (v == -1) {
      resultBuilder += ((Option.empty, List(ToLastAction(g))))
    }
    if (v != 0) {
      if (numConstraints.contains(v.abs)) {
        resultBuilder += ((Some(v.abs), List(if (v > 0) ForwardAction(v, g) else BackwardAction(-v, g))))
      }
      if (numConstraints.contains(v.abs - 1)) {
        if (v > 0) resultBuilder += ((Some(v.abs - 1), List(ForwardAction(v.abs - 1, g), ToNextAction(g))))
        else resultBuilder += ((Some(v.abs - 1), List(BackwardAction(v.abs - 1, g), ToLastAction(g))))
      }
      if (numConstraints.contains(v.abs + 1)) {
        if (v > 0) resultBuilder += ((Some(v.abs + 1), List(ForwardAction(v.abs + 1, g), ToLastAction(g))))
        else resultBuilder += ((Some(v.abs + 1), List(BackwardAction(v.abs + 1, g), ToNextAction(g))))
      }
    }
    resultBuilder.result()
  }

  def checkModify(v: Int, g: TimeUnit, up: TimeUnit, t: TimePointImpl): Array[(Option[Int], List[Action])] = {
    val resultBuilder = mutable.ArrayBuilder.make[(Option[Int], List[Action])]()

    if (g == TimeUnit.Season)
      resultBuilder += ((Option.empty, List(new ModifyEnumAction(v, Season))))
    if (g == TimeUnit.POD)
      resultBuilder += ((Option.empty, List(new ModifyEnumAction(v, TimeOfDay))))
    if (g == TimeUnit.Month && (up <= TimeUnit.Year || up == TimeUnit.Unknown))
      resultBuilder += ((Option.empty, List(new ModifyEnumAction(v, MonthOfYear))))
    if (g == TimeUnit.Day && up == TimeUnit.Week)
      resultBuilder += ((Option.empty, List(new ModifyEnumAction(v, DayOfWeek))))

    val range = t.getRange(g, up)
    def useable(v: Int) = {
      val seenNumber = numConstraints.contains(v) || (g == TimeUnit.Day && up == TimeUnit.Month)
      seenNumber && range.nonEmpty && !(range.get._1 > v) && !(range.get._2 < v)
    }

    if (range.nonEmpty && Seq(Season, TimeOfDay, MonthOfYear, DayOfWeek).forall(e => e.grain != g || e.upperGrain != up)) {
      if (range.get._1 == v)
        resultBuilder += ((Option.empty, List(ToBeginAction(g, Some(up)))))
      if (range.get._2 == v)
        resultBuilder += ((Option.empty, List(ToEndAction(g, Some(up)))))
    }
    if (useable(v)) {
      resultBuilder += ((Some(v), List(ModifyValAction(UInt(v), g, Option(up)))))
    }
    if (v - 1 != 0 && useable(v - 1)) {
      resultBuilder += ((Some(v - 1), List(ModifyValAction(UInt(v - 1), g, Option(up)), ToNextAction(g))))
    }
    if (useable(v + 1)) {
      resultBuilder += ((Some(v + 1), List(ModifyValAction(UInt(v + 1), g, Option(up)), ToLastAction(g))))
    }
    resultBuilder.result()
  }

  def checkSetOrUnknown(isSet: Boolean, g: TimeUnit, up: TimeUnit): Array[(Option[Int], List[Action])] = {
    val resultBuilder = mutable.ArrayBuilder.make[(Option[Int], List[Action])]()
    if (isSet)
      resultBuilder += ((Option.empty, List(MakeSetAction(g))))

    if (TimeUnit.defaultUpperGrain(g) == up)
      resultBuilder += ((Option.empty, List(ModifyValAction(UInt.unknownInt, g, Some(up)))))

    resultBuilder.result()
  }

  def checkCount(target: TimePointImpl, e: EnumConstants, up: TimeUnit): Array[(Option[Int], List[Action])] = {
    val v = target.get(e.grain, e.upperGrain)
    if (v.isEmpty || v.get.isUnknown)
      return Array.empty
    val cnt = target.getCount(e, up)
    if (cnt.isKnown && numConstraints.contains(cnt.value)) {
      Array((Some(cnt.value), List(CountAction(cnt.value, e(v.get.value), e, up))))
    } else {
      Array.empty
    }
  }

  var depth = 0
  /**
   *
   * @param target
   * @param current
   * @param finishedGrain
   * @param currentGrain
   * @param upperGrain
   * @param lastGrain
   * @param canUseEqual
   * @param hasActions
   * @return
   */
  def inferDateTime(target: TimePointImpl, current: TimePointImpl, finishedGrain: TimeUnit, currentGrain: TimeUnit, upperGrain: TimeUnit, lastGrain: Option[TimeUnit], canUseEqual: Boolean, hasActions: Int): SolutionSet = {

/*
    if (finishedGrain >= TimeUnit.Day) {
      println("\t"*depth, target.toTimex3Fmt, current.toTimex3Fmt, finishedGrain, currentGrain, upperGrain, lastGrain, upperGrain == TimeUnit.defaultUpperGrain(currentGrain), canUseEqual)
      println(target.smallestGrain, TimeUnit.smallerOrEqual(finishedGrain, target.smallestGrain))
    }
*/

    if (TimeUnit.smallerOrEqual(upperGrain, currentGrain) || TimeUnit.smallerThan(upperGrain, finishedGrain) || TimeUnit.smallerOrEqual(upperGrain, TimeUnit.Unknown))
      return SolutionSet.noSolution
    if (hasActions > actionLimit)
      return SolutionSet.noSolution
    if (hasActions > 0 && TimeUnit.smallerOrEqual(finishedGrain, target.smallestGrain))
      return SolutionSet.successMark
    if (TimeUnit.smallerThan(currentGrain, target.smallestGrain) || currentGrain == TimeUnit.Unknown)
      if (TimeUnit.smallerOrEqual(finishedGrain, target.smallestGrain)) return SolutionSet.successMark
      else return SolutionSet.noSolution

    if (defaultActionIndex.contains(currentGrain, upperGrain)) {
      return iterCheckResult(Array((Option.empty, defaultActionIndex((currentGrain, upperGrain)))))
    }

    depth += 1

    def iterCheckResult(checkResult: Array[(Option[Int], List[Action])]): SolutionSet = {
      val result = new SolutionSet()
      checkResult.foreach(vp => {
        val usedNum = vp._1
        val actions = vp._2
        if (usedNum.isDefined) numConstraints.use(usedNum.get)
        val newCurrent = current.copy
        actions.foreach(newCurrent.executeAction)
        result ++= (inferDateTime(
          target,
          newCurrent,
          finishedGrain = TimeUnit.smaller(currentGrain, finishedGrain),
          currentGrain = TimeUnit.next(currentGrain),
          upperGrain = currentGrain,
          lastGrain = Some(currentGrain),
          canUseEqual = true,
          hasActions = hasActions + actions.length
        ) x actions)
        if (usedNum.isDefined) numConstraints.add(usedNum.get)
      })
      result
    }

    val result = new SolutionSet()

    val tValueOp = target.get(currentGrain, upperGrain)

    if (tValueOp.nonEmpty) {
      val tValue = tValueOp.get

      val upperIsKnown =
        (lastGrain.nonEmpty && lastGrain.get == upperGrain) ||
        ((lastGrain.isEmpty || TimeUnit.smallerThan(lastGrain.get, upperGrain)) && upperGrain == TimeUnit.defaultUpperGrain(currentGrain))

      if (currentGrain != TimeUnit.Season && currentGrain != TimeUnit.POD && upperIsKnown) {
        val cValueOp = current.get(currentGrain, upperGrain)
        if (cValueOp.nonEmpty && currentGrain != TimeUnit.POD && currentGrain != TimeUnit.Season) {
          val cValue = cValueOp.get
          val diff = tValue - cValue

          if (diff == 0 && canUseEqual) {
            result ++= (inferDateTime(
              target,
              current,
              finishedGrain = TimeUnit.smaller(currentGrain, finishedGrain),
              currentGrain = TimeUnit.next(currentGrain),
              upperGrain = currentGrain,
              lastGrain = Some(currentGrain),
              canUseEqual = false,
              hasActions = hasActions + 1
            ) x EqualAction(currentGrain))
          } else if (diff.isKnown) {
            result ++= iterCheckResult(checkOffset(diff.value, currentGrain))
          }
        }
      }

      if (tValue.isKnown && (currentGrain != TimeUnit.Season || tValue.value != Season.Unknown.id) && (currentGrain != TimeUnit.POD || tValue.value != TimeOfDay.Unknown.id)) {
        if (hasActions == 0 || upperIsKnown) {
          result ++= iterCheckResult(checkModify(tValue.value, currentGrain, upperGrain, current))
          Array(Season, MonthOfYear, DayOfWeek, TimeOfDay).foreach(enum => {
            if (enum.grain == currentGrain && TimeUnit.smallerThan(enum.upperGrain, upperGrain))
              result ++= iterCheckResult(checkCount(target, enum, upperGrain))
          })
        }
      } else if (target.unknownGrain contains currentGrain)
        result ++= iterCheckResult(checkSetOrUnknown(target.isSet, currentGrain, upperGrain))
    }

    result ++= inferDateTime(target, current, finishedGrain, TimeUnit.next(currentGrain), upperGrain, lastGrain, canUseEqual, hasActions)
    if (hasActions == 0 && tValueOp.nonEmpty)
      result ++= inferDateTime(target, current, finishedGrain, currentGrain, TimeUnit.next(upperGrain), lastGrain, canUseEqual, hasActions)
    else if (upperGrain != TimeUnit.defaultUpperGrain(currentGrain) && TimeUnit.smallerThan(TimeUnit.defaultUpperGrain(currentGrain), upperGrain))
      result ++= inferDateTime(target, current, finishedGrain, currentGrain, TimeUnit.defaultUpperGrain(currentGrain), lastGrain, canUseEqual, hasActions)

    depth -= 1
/*
    if (finishedGrain >= TimeUnit.Day) {
      println("\t" * depth, result)
    }
*/
    result
  }

  def inferPoint(value: FuzzyTimePoint) = {
    val target = value.asInstanceOf[TimePointImpl]
    val startGrain = TimeUnit.values.-(TimeUnit.Forever).collectFirst {
      case t if !target.unknownGrain.contains(t) => t
    }.getOrElse(TimeUnit.Unknown)
    val checkedLast = TimeUnit.values.-(TimeUnit.Forever).takeWhile(t => {
      val hasGrain = DateForm.hasGrain(target.dateForm, t) || TimeForm.hasGrain(target.timeForm, t)
      !hasGrain || (TimeUnit.smallerOrEqual(target.smallestGrain, t) && {
        val tV = target.get(t, TimeUnit.defaultUpperGrain(t))
        val cV = refPoint.get(t, TimeUnit.defaultUpperGrain(t))
        tV.isDefined && tV.get.isKnown && tV == cV
      })
    }).lastOption
//    println(startGrain, checkedLast)
    inferDateTime(
      target,
      refPoint,
      finishedGrain = checkedLast.getOrElse(TimeUnit.Forever),
      currentGrain = startGrain,
      upperGrain = TimeUnit.Forever,
      lastGrain = Option.empty,
      canUseEqual = true,
      hasActions = 0
    )
  }

  def inferDuration(duration: TimeDuration): SolutionSet = {
    if (duration.unknown)
      new SolutionSet(AddToAction(UInt.unknownInt, TimeUnit.Unknown))
    else {
      val tmp: Seq[AddToAction] = TimeUnit.values.withFilter(g => duration.values.contains(g)).map(g => {
        AddToAction(duration.values(g), g)
      }).toSeq
      val results: Seq[Action] =
        if (duration.isSet)
          if (tmp.exists(a => a.v.isUnknown))
            tmp.map(a => if (a.v.isUnknown) MakeSetAction(a.g) else a)
          else
            tmp :+ MakeSetAction(tmp.last.g)
      else tmp
      new SolutionSet(results: _*)
    }
  }

  def getNumberOfTimeUnits(actions: Seq[Action]): Int = {
    actions.collect {
      case a: AddToAction  => List(a.g)
      case a: OffsetAction => List(a.g)
      case a: ModifyAction => if (a.up.nonEmpty) List(a.g, a.up.get) else List(a.g)
      case a: CountAction => List(a.g.grain, a.g.upperGrain, a.up)
      case a: MakeSetAction => List(a.g)
    }.flatten.filter(_ != TimeUnit.Unknown).toSet.size
  }

  def solutionFilter(solutions: SolutionSet, needSG: Boolean, sg: TimeUnit, isSet: Boolean): SolutionSet = {
    def hasProperSubSolution(target: Seq[Action], solutionSet: mutable.Set[List[Action]]): Boolean = {
      def looseUp(a: Action): Action = {
        a match {
          case a: ModifyAction => ModifyValAction(a.v, a.g, Option.empty)
          case a => a
        }
      }
      val s = target.map(looseUp).toSet
      solutionSet.exists(p => p.nonEmpty && p.map(looseUp).toSet != s && p.map(looseUp).forall(s.contains))
    }

    val cFiltered = solutions.solutions
      .filter(_.nonEmpty)
      .filter(s => defaultActions.forall(s.contains))
      .map(s => s.diff(defaultActions))
      .filter(s => !isSet || s.exists(a => a.isInstanceOf[MakeSetAction]))
      .filter(s => sg == TimeUnit.Unknown || !needSG || s.exists {
        case a: AddToAction  => a.g == sg
        case a: OffsetAction => a.g == sg
        case a: ModifyAction => a.g == sg
        case a: MakeSetAction => a.g == sg
        case a: CountAction => a.g.grain == sg
        case _: RefAction => true
      })
      .filter(s => sg == TimeUnit.Unknown || s.forall {
        case a: AddToAction  => a.g <= sg
        case a: OffsetAction => a.g <= sg
        case a: ModifyAction => a.g <= sg
        case a: MakeSetAction => a.g <= sg
        case a: CountAction => a.g.grain <= sg
        case _: RefAction => true
      })
    val filtered = cFiltered
//      .filter(s => !hasProperSubSolution(s, cFiltered))

//    println(solutions, filtered)

    if (filtered.size < solutions.solutions.size || defaultActionIndex.nonEmpty)
      new SolutionSet(filtered)
    else
      solutions
  }

  def inference(value: TimeValue): SolutionSet = {
    val solutions = value match {
      case x: TimeRef => inferRef(x.asInstanceOf[TimeRef])
      case x: FuzzyTimePoint => inferPoint(x.asInstanceOf[FuzzyTimePoint])
      case x: TimeDuration => inferDuration(x.asInstanceOf[TimeDuration])
    }
    val sg =
      value match {
        case p: TimePointImpl => p.smallestGrain
        case _ => TimeUnit.Unknown
      }
//    val needSG = refPoint.smallestGrain != sg
    val isSet = value.isSet
    solutionFilter(solutions, true, sg, isSet)
  }
}
