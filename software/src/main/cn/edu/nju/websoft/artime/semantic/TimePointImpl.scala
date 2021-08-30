package cn.edu.nju.websoft.artime.semantic

import java.time.LocalDateTime

import cn.edu.nju.websoft.artime.action._
import cn.edu.nju.websoft.artime.io.TIMEX3Parser
import cn.edu.nju.websoft.artime.semantic.constant.DateForm.{DateForm => _, _}
import cn.edu.nju.websoft.artime.semantic.constant.Direction.Direction
import cn.edu.nju.websoft.artime.semantic.constant.Season.Season
import cn.edu.nju.websoft.artime.semantic.constant.TimeForm.{DT, H, HM, HMS}
import cn.edu.nju.websoft.artime.semantic.constant.TimeOfDay.TimeOfDay
import cn.edu.nju.websoft.artime.semantic.constant.TimeUnit.TimeUnit
import cn.edu.nju.websoft.artime.semantic.constant.{TimeUnit, _}
import cn.edu.nju.websoft.artime.utils.UInt
import edu.stanford.nlp.time.SUTime
import org.json4s.JsonDSL._
import org.json4s.native.JsonMethods.{compact, render}

class TimePointImpl private (
  var lBound: LocalDateTime,
  var rBound: LocalDateTime,
  var smallestGrain: TimeUnit.Value,
  var season: Season.Value,
  var TOD: TimeOfDay.Value,
  var dateForm: DateForm.Value,
  var timeForm: TimeForm.Value,
  var unknownGrain: Set[TimeUnit],
  var isSet: Boolean,
  var tid : Int,
  var timex3type: String
) extends FuzzyTimePoint with FuzzyDate with FuzzyTime {
  def this() = this(
    lBound = LocalDateTime.MIN.plusYears(999),
    rBound = LocalDateTime.MAX.withYear(9999999),
    smallestGrain = TimeUnit.Unknown,
    season = Season.Unknown,
    TOD = TimeOfDay.Unknown,
    dateForm = DateForm.Empty,
    timeForm = TimeForm.Empty,
    unknownGrain = Set.empty,
    isSet = false,
    tid =0,
    timex3type= "DATE"
  )

  def copy = new TimePointImpl(
    this.lBound,
    this.rBound,
    this.smallestGrain,
    this.season,
    this.TOD,
    this.dateForm,
    this.timeForm,
    this.unknownGrain,
    this.isSet,
    this.tid,
    this.timex3type
  )

  def updateSmallestGrain(g: TimeUnit) = {
    if (TimeUnit.isDate(g)) {
      g match {
        case TimeUnit.Millennium | TimeUnit.Century | TimeUnit.Decade | TimeUnit.Year => dateForm = DateForm.Y
        case TimeUnit.HalfY => dateForm = DateForm.YH
        case TimeUnit.QuarterY => dateForm = DateForm.YQ
        case TimeUnit.Season => dateForm = DateForm.YS
        case TimeUnit.Month => dateForm = DateForm.YM
        case TimeUnit.Week => dateForm = DateForm.YW
        case TimeUnit.Day => dateForm = DateForm.YMD
      }
      timeForm = TimeForm.Empty
    }
    if (TimeUnit.isTime(g)) {
      g match {
        case TimeUnit.POD => timeForm = TimeForm.DT
        case TimeUnit.HalfD | TimeUnit.Hour => timeForm = TimeForm.H
        case TimeUnit.Minute => timeForm = TimeForm.HM
        case TimeUnit.Second => timeForm = TimeForm.HMS
      }
    }
    smallestGrain = g
  }


  def executeAction(a: Action): Unit = {
    def updateForm(g: TimeUnit, forced: Boolean): Unit = {
      if (TimeUnit.isDate(g) && g != TimeUnit.Forever) {
        if (forced || !DateForm.hasGrain(dateForm, g)) {
          g match {
            case TimeUnit.Millennium | TimeUnit.Century | TimeUnit.Decade | TimeUnit.Year => dateForm = DateForm.Y
            case TimeUnit.HalfY => dateForm = DateForm.YH
            case TimeUnit.QuarterY => dateForm = DateForm.YQ
            case TimeUnit.Season => dateForm = DateForm.YS
            case TimeUnit.Month => dateForm = DateForm.YM
            case TimeUnit.Week => dateForm = DateForm.YW
            case TimeUnit.Day => dateForm match {
              case DateForm.Y => dateForm = DateForm.YD
              case DateForm.YM => dateForm = DateForm.YMD
              case DateForm.YW => dateForm = DateForm.YWU
              case _ => dateForm = DateForm.YMD
            }
          }
        }
      }
      if (TimeUnit.isTime(g)) {
        if (forced || !TimeForm.hasGrain(timeForm, g)) {
          timex3type="TIME"
          g match {
            case TimeUnit.POD => timeForm = TimeForm.DT
            case TimeUnit.HalfD | TimeUnit.Hour => timeForm = TimeForm.H
            case TimeUnit.Minute => timeForm = TimeForm.HM
            case TimeUnit.Second => timeForm = TimeForm.HMS
          }
        }
      }
      if (smallestGrain < g)
        smallestGrain = g
    }

    a match {
      case a: ModifyAction =>
        if (a.up.isDefined)
          updateForm(a.up.get, forced = true)
        updateForm(a.g, a.v.isUnknown)
        executeModify(a.v, a.g, a.up.getOrElse(TimeUnit.defaultUpperGrain(a.g)))
      case a: OffsetAction =>
        updateForm(a.g, forced = false)
        executeOffset(a.v, a.g, a.d)
      case a: MakeSetAction =>
        updateForm(a.g, forced = true)
        unknownGrain ++= TimeUnit.values.drop(1).takeWhile(t => TimeUnit.smallerOrEqual(a.g, t))
        isSet = true
      case a: CountAction =>
        updateForm(a.g.grain, forced = false)
        executeCount(a.v, a.e, a.g, a.up)
      case a: RefAction => //TODO
      //TODO
    }
  }

  def executeOffset(v: Int, g: TimeUnit, d: Direction): Unit = {
    val delta = if (d == Direction.Past) -v else v
    g match {
      case TimeUnit.Millennium => plusMillennium(delta)
      case TimeUnit.Century => plusCentury(delta)
      case TimeUnit.Decade => plusDecade(delta)
      case TimeUnit.Year => plusYear(delta)
      case TimeUnit.HalfY => plusHalfY(delta)
      case TimeUnit.QuarterY => plusQuarterY(delta)
      case TimeUnit.Season => plusSeason(delta)
      case TimeUnit.Month => plusMonth(delta)
      case TimeUnit.Week => plusWeek(delta)
      case TimeUnit.Day => plusDay(delta)
      case TimeUnit.HalfD => plusHalfD(delta)
      case TimeUnit.POD => TOD = TimeOfDay.Unknown
      case TimeUnit.Hour => plusHour(delta)
      case TimeUnit.Minute => plusMinute(delta)
      case TimeUnit.Second => plusSecond(delta)
    }
  }

  def executeModify(v: UInt, g: TimeUnit, up: TimeUnit): Unit = {
    if (v.isUnknown) {
      unknownGrain += g
    } else {
      val value: Int = v.value match {
        case -1 => getRange(g, up).get._2.value //Last
        case -2 => getRange(g, up).get._1.value //First
        case x => x
      }
      unknownGrain -= g
      g match {
        case TimeUnit.Millennium => setMillennium(value)
        case TimeUnit.Century => setCentury(value, up)
        case TimeUnit.Decade => setDecade(value, up)
        case TimeUnit.Year => setYear(value, up)
        case TimeUnit.HalfY => setHalfY(value)
        case TimeUnit.QuarterY => setQuarterY(value, up)
        case TimeUnit.Season => season = Season(value)
        case TimeUnit.Month => setMonth(value, up)
        case TimeUnit.Week => setWeek(value, up)
        case TimeUnit.Day => setDay(value, up)
        case TimeUnit.HalfD => setHalfD(value)
        case TimeUnit.POD => TOD = TimeOfDay(value)
        case TimeUnit.Hour => setHour(value,up)
        case TimeUnit.Minute => setMinute(value)
        case TimeUnit.Second => setSecond(value)
      }
    }
  }

  //TODO: only forever now
  private def setMillennium(v: Int): Unit = {
    lBound = lBound.withYear((lBound.getYear % 1000 + 1000) % 1000 + v * 1000)
    rBound = rBound.withYear((rBound.getYear % 1000 + 1000) % 1000 + v * 1000)
  }

  private def plusMillennium(v: Int): Unit = {
    lBound = lBound.plusYears(v * 1000)
    rBound = rBound.plusYears(v * 1000)
  }

  //TODO: only forever now
  private def setCentury(v: Int, up: TimeUnit): Unit = {
    up match {
      case TimeUnit.Millennium =>
        lBound = lBound.withYear(lBound.getYear / 1000 * 1000 + lBound.getYear % 100+ (v - 1) * 100)
        rBound = rBound.withYear(rBound.getYear / 1000 * 1000 + lBound.getYear % 100 + (v - 1) * 100)
      case TimeUnit.Forever =>
        lBound = lBound.withYear(lBound.getYear % 100 + (v - 1) * 100)
        rBound = rBound.withYear(rBound.getYear % 100 + (v - 1) * 100)
    }
  }

  private def plusCentury(v: Int): Unit = {
    lBound = lBound.plusYears(v * 100)
    rBound = rBound.plusYears(v * 100)
  }

  private def setDecade(v: Int, up: TimeUnit): Unit = {
//    println(up.toString)
    up match {
      case TimeUnit.Forever =>
        lBound = lBound.withYear(lBound.getYear % 10 + v * 10)
        rBound = rBound.withYear(rBound.getYear % 10 + v * 10)
      case TimeUnit.Millennium =>
        lBound = lBound.withYear(lBound.getYear / 1000 * 1000 + lBound.getYear % 10 + v * 10)
        rBound = rBound.withYear(rBound.getYear / 1000 * 1000 + rBound.getYear % 10 + v * 10)
      case TimeUnit.Century =>
        lBound = lBound.withYear(lBound.getYear / 100 * 100 + lBound.getYear % 10 + v * 10)
        rBound = rBound.withYear(rBound.getYear / 100 * 100 + rBound.getYear % 10 + v * 10)
    }
  }

  private def plusDecade(v: Int): Unit = {
    lBound = lBound.plusYears(v * 10)
    rBound = rBound.plusYears(v * 10)
  }

  private def setYear(v: Int, up: TimeUnit): Unit = {
    val m = up match {
      case TimeUnit.Millennium => 1000
      case TimeUnit.Century => 100
      case TimeUnit.Decade => 10
      case /*Forever*/_ => 0
    }
    if (m > 0) {
      lBound = lBound.withYear(lBound.getYear / m * m + v)
      rBound = rBound.withYear(rBound.getYear / m * m + v)
    }
    else {
      lBound = lBound.withYear(v)
      rBound = rBound.withYear(v)
    }
  }

  private def plusYear(v: Int): Unit = {
    lBound = lBound.plusYears(v)
    rBound = rBound.plusYears(v)
  }

  private def setHalfY(v: Int): Unit = {
    lBound = lBound.withMonth(v * 6 - 5)
    rBound = rBound.withMonth(v * 6)
  }

  private def plusHalfY(v: Int): Unit = {
    lBound = lBound.plusMonths(v * 6)
    rBound = rBound.plusMonths(v * 6)
  }

  private def setQuarterY(v: Int, up: TimeUnit): Unit = {
    up match {
      case TimeUnit.HalfY =>
        lBound = lBound.withMonth((lBound.getMonth.getValue - 1) / 6 * 6 + v * 3 - 2)
        rBound = rBound.withMonth((rBound.getMonth.getValue - 1) / 6 * 6 + v * 3)
      case /*Year*/_ =>
        lBound = lBound.withMonth(v * 3 - 2)
        rBound = rBound.withMonth(v * 3)
    }
  }

  private def plusQuarterY(v: Int): Unit = {
    lBound = lBound.plusMonths(v * 3)
    rBound = rBound.plusMonths(v * 3)
  }

  private def plusSeason(v: Int): Unit = {
    val c = if (this.season == Season.Unknown) 0 else this.season.id
    lBound = lBound.plusYears((v + c - 1)/ 4)
    rBound = rBound.plusMonths((v + c - 1)/ 4)
    if (this.season != Season.Unknown)
      season = Season((v + c - 1) % 4 + 1)
  }

  private def setMonth(v: Int, up: TimeUnit): Unit = {
    val m = up match {
      case TimeUnit.QuarterY => 3
      case TimeUnit.HalfY => 6
      case /*Year*/_ => 0
    }
    if (m > 0) {
      lBound = lBound.withMonth((lBound.getMonth.getValue - 1) / m * m + v)
      rBound = rBound.withMonth((rBound.getMonth.getValue - 1) / m * m + v)
    } else {
      lBound = lBound.withMonth(v)
      rBound = rBound.withMonth(v)
    }
  }

  private def plusMonth(v: Int): Unit = {
    lBound = lBound.plusMonths(v)
    rBound = rBound.plusMonths(v)
  }

  private def setWeek(v: Int, up: TimeUnit.TimeUnit): Unit = {
    up match {
      case TimeUnit.Month =>
        val offset = lBound.withDayOfMonth(1).getDayOfWeek.getValue - 1
        lBound = lBound.withDayOfMonth(Math.max(1, v * 7 - offset - 6))
        rBound = rBound.withDayOfMonth(Math.min(lBound.plusMonths(1).minusDays(1).getDayOfMonth, v * 7 - offset))
      case /*Year*/_ =>
        val offset = lBound.withDayOfYear(1).getDayOfWeek.getValue - 1
        lBound = lBound.withDayOfYear(Math.max(1, v * 7 - offset - 6))
        rBound = rBound.withDayOfYear(Math.min(if (rBound.toLocalDate.isLeapYear) 366 else 365, v * 7 - offset))
    }
  }

  private def plusWeek(v: Int): Unit = {
    lBound = lBound.plusDays(v * 7)
    rBound = rBound.plusDays(v * 7)
  }

  private def setDay(v: Int, up: TimeUnit.TimeUnit): Unit = {
    up match {
      case TimeUnit.Week =>
        if (v == DayOfWeek.Weekend.id) {
          lBound = lBound.plusDays(6 - lBound.getDayOfWeek.getValue)
          rBound = rBound.plusDays(7 - rBound.getDayOfWeek.getValue)
        } else {
          lBound = lBound.plusDays(v - lBound.getDayOfWeek.getValue)
          rBound = rBound.plusDays(v - rBound.getDayOfWeek.getValue)
        }
      case TimeUnit.Year =>
        lBound = lBound.withDayOfYear(v)
        rBound = rBound.withDayOfYear(v)
      case TimeUnit.Month =>
        lBound = lBound.withDayOfMonth(v)
        rBound = rBound.withDayOfMonth(v)
    }
  }

  private def plusDay(v: Int): Unit = {
    lBound = lBound.plusDays(v)
    rBound = rBound.plusDays(v)
  }

  private def setHalfD(halfD: Int): Unit = {
    lBound = lBound.withHour((halfD - 1) * 12)
    rBound = lBound.plusHours(12).minusNanos(1)
  }

  private def plusHalfD(v: Int): Unit = {
    lBound = lBound.plusHours(v * 12)
    rBound = rBound.plusHours(v * 12)
  }

  private def setHour(hour: Int): Unit = {
    lBound = lBound.withHour(hour % 24)
    rBound = lBound.plusHours(1).minusNanos(1)
  }

  private def setHour(hour: Int, up: TimeUnit): Unit = {
    up match {
      case TimeUnit.HalfD =>
        lBound = lBound.withHour(lBound.getHour / 12 * 12 + hour % 24)
        rBound = lBound.plusHours(1).minusNanos(1)
      case TimeUnit.Day | TimeUnit.Unknown =>
        lBound = lBound.withHour(hour % 24)
        rBound = lBound.plusHours(1).minusNanos(1)
    }
  }

  private def plusHour(v: Int): Unit = {
    lBound = lBound.plusHours(v)
    rBound = rBound.plusHours(v)
  }

  private def setMinute(minute: Int): Unit = {
    lBound = lBound.withMinute(minute)
    rBound = lBound.plusMinutes(1).minusNanos(1)
  }

  private def plusMinute(v: Int): Unit = {
    lBound = lBound.plusMinutes(v)
    rBound = rBound.plusMinutes(v)
  }

  private def setSecond(second: Int): Unit = {
    lBound = lBound.withSecond(second)
    rBound = lBound.plusSeconds(1).minusNanos(1)
  }

  private def plusSecond(v: Int): Unit = {
    lBound = lBound.plusSeconds(v)
    rBound = rBound.plusSeconds(v)
  }

  private def calcWeekOfYear(ldt: LocalDateTime): Int = {
    val offset = ldt.withDayOfYear(1).getDayOfWeek.getValue - 1
    Math.max(1, (ldt.getDayOfYear + offset - 1) / 7 + 1)
  }

  private def calcWeekOfMonth(ldt: LocalDateTime): Int = {
    val offset = ldt.withDayOfMonth(1).getDayOfWeek.getValue - 1
    Math.max(1, (ldt.getDayOfMonth + offset - 1) / 7 + 1)
  }

  def getRange(g: TimeUnit, up: TimeUnit): Option[(UInt, UInt)] = {
    val lR = getRange(lBound, g, up)
    val rR = getRange(rBound, g, up)
    if (lR.isEmpty || rR.isEmpty)
      Option.empty
    else {
      Some(if (lR.get._1 > rR.get._1) lR.get._1 else rR.get._1, if (lR.get._2 < rR.get._2) lR.get._2 else rR.get._2)
    }
  }

  def moveToFirst(e: Int, g: EnumConstants, up: TimeUnit) = {
    /*val startPoint = this.copy*/
    val start = getRange(g.upperGrain, up).get._1
    /*startPoint.*/executeModify(start, g.upperGrain, up)
    /*startPoint.*/executeModify(UInt(1), g.grain, TimeUnit.defaultUpperGrain(g.grain))
    val startV = /*startPoint.*/get(g.grain, g.upperGrain).get.value
    if (e < startV) {
      /*startPoint.*/executeOffset(1, g.upperGrain, Direction.Future)
      /*startPoint.*/executeOffset(startV - e, g.grain, Direction.Past)
    } else {
      /*startPoint.*/executeOffset(e - startV, g.grain, Direction.Future)
    }
    /*startPoint*/
  }

  def getCount(g: EnumConstants, up: TimeUnit): UInt = {
    val v = this.get(up, TimeUnit.Unknown)
    if (v.isEmpty)
      return UInt.unknownInt
    var cnt = 0
    val point = this.copy
    do {
      point.executeOffset(1, g.upperGrain, Direction.Past)
      cnt += 1
    } while(point.get(up, TimeUnit.Unknown) == v)

    UInt(cnt)
  }

  def executeCount(v: Int, e: Enumeration#Value, g: EnumConstants, up: TimeUnit) = {
    /*val point = */if (e == DayOfWeek.Weekend) {
      moveToFirst(6, g, up)
    } else {
      moveToFirst(e.id, g, up)
    }
    (1 until v).foreach(_ => /*point.*/executeOffset(1, g.upperGrain, Direction.Future))
    if (e == DayOfWeek.Weekend) {
      /*point.*/executeModify(UInt(e.id), TimeUnit.Day, TimeUnit.Week)
    }
    /*point*/
  }

  def getRange(ldt: LocalDateTime, g: TimeUnit, up: TimeUnit): Option[(UInt, UInt)] = {
    g match {
      case TimeUnit.Millennium =>
        Some((UInt.unknownInt, UInt.unknownInt))
      case TimeUnit.Century =>
        up match {
          case TimeUnit.Millennium =>
            Some((UInt(0), UInt(9)))
          case TimeUnit.Unknown | TimeUnit.Forever =>
            Some((UInt.unknownInt, UInt.unknownInt))
          case _ =>
            Option.empty
        }
      case TimeUnit.Decade =>
        up match {
          case TimeUnit.Millennium =>
            Some((UInt(0), UInt(99)))
          case TimeUnit.Century =>
            Some((UInt(0), UInt(9)))
          case TimeUnit.Unknown | TimeUnit.Forever =>
            Some((UInt.unknownInt, UInt.unknownInt))
          case _ =>
            Option.empty
        }
      case TimeUnit.Year =>
        up match {
          case TimeUnit.Millennium =>
            Some((UInt(0), UInt(999)))
          case TimeUnit.Century =>
            Some((UInt(0), UInt(99)))
          case TimeUnit.Decade =>
            Some((UInt(0), UInt(9)))
          case TimeUnit.Unknown | TimeUnit.Forever =>
            Some((UInt.unknownInt, UInt.unknownInt))
          case _ => UInt.unknownInt
            Option.empty
        }
      case TimeUnit.HalfY =>
        up match {
          case TimeUnit.Year | TimeUnit.Unknown =>
            Some((UInt(1), UInt(2)))
          case _ =>
            Option.empty
        }
      case TimeUnit.Season =>
        up match {
          case TimeUnit.Year | TimeUnit.Unknown =>
            Some((UInt(1), UInt(4)))
          case _ =>
            Option.empty
        }
      case TimeUnit.QuarterY =>
        up match {
          case TimeUnit.HalfY =>
            Some((UInt(1), UInt(2)))
          case TimeUnit.Unknown | TimeUnit.Year =>
            Some((UInt(1), UInt(4)))
          case _ =>
            Option.empty
        }
      case TimeUnit.Month =>
        up match {
          case TimeUnit.HalfY =>
            Some((UInt(1), UInt(6)))
          case TimeUnit.QuarterY =>
            Some((UInt(1), UInt(3)))
          case TimeUnit.Unknown | TimeUnit.Year =>
            Some((UInt(1), UInt(12)))
          case _ =>
            Option.empty
        }
      case TimeUnit.Week =>
        up match {
          case TimeUnit.Month =>
            val r = calcWeekOfMonth(ldt.withDayOfMonth(ldt.getMonth.length(ldt.toLocalDate.isLeapYear)))
            Some((UInt(1), UInt(r)))
          case TimeUnit.Unknown | TimeUnit.Year =>
            val r = calcWeekOfYear(ldt.withDayOfYear(if (ldt.toLocalDate.isLeapYear) 366 else 365))
            Some((UInt(1), UInt(r)))
          case _ =>
            Option.empty
        }
      case TimeUnit.Day =>
        up match {
          case TimeUnit.Week =>
            Some((UInt(1), UInt(7)))
          case TimeUnit.Unknown | TimeUnit.Year =>
            val r = if (ldt.toLocalDate.isLeapYear) 366 else 365
            Some((UInt(1), UInt(r)))
          case TimeUnit.Month =>
            val r = ldt.getMonth.length(ldt.toLocalDate.isLeapYear)
            Some((UInt(1), UInt(r)))
          case _ =>
            Option.empty
        }
      case TimeUnit.HalfD =>
        up match {
          case TimeUnit.Unknown | TimeUnit.Day =>
            Some((UInt(1), UInt(2)))
          case _ =>
            Option.empty
        }
      case TimeUnit.POD =>
          Some(UInt(1), UInt(TimeOfDay.values.size - 1))
      case TimeUnit.Hour =>
        up match {
          case TimeUnit.Unknown | TimeUnit.Day =>
            Some((UInt(0), UInt(23)))
          case TimeUnit.HalfD =>
            Some((UInt(0), UInt(11)))
          case _ =>
            Option.empty
        }
      case TimeUnit.Minute =>
        up match {
          case TimeUnit.Unknown | TimeUnit.Hour =>
            Some((UInt(0), UInt(59)))
          case _ =>
            Option.empty
        }
      case TimeUnit.Second => UInt(ldt.getSecond)
        up match {
          case TimeUnit.Unknown | TimeUnit.Minute =>
            Some((UInt(0), UInt(59)))
          case _ =>
            Option.empty
        }
    }
  }

  /**
   * 再给定上位单位的前提下查询值
   * @param ldt 查询的ldt
   * @param g 查询单位
   * @param up 上位单位，为Unknown时返回缺省单位
   * @return 无法查值时返回Unknown
   */
  def get(ldt: LocalDateTime, g: TimeUnit, up: TimeUnit): UInt = {
    g match {
      case TimeUnit.Millennium =>
        UInt(ldt.getYear / 1000)
      case TimeUnit.Century =>
        up match {
          case TimeUnit.Millennium => UInt(ldt.getYear % 1000 / 100) + 1
          case TimeUnit.Unknown | TimeUnit.Forever => UInt(ldt.getYear / 100) + 1
          case _ => UInt.unknownInt
        }
      case TimeUnit.Decade =>
        up match {
          case TimeUnit.Millennium => UInt(ldt.getYear % 1000 / 10) + 1
          case TimeUnit.Century => UInt(ldt.getYear % 100 / 10)
          case TimeUnit.Unknown | TimeUnit.Forever => UInt(ldt.getYear / 10)
          case _ => UInt.unknownInt
        }
      case TimeUnit.Year =>
        up match {
          case TimeUnit.Millennium => UInt(ldt.getYear % 1000)
          case TimeUnit.Century => UInt(ldt.getYear % 100)
          case TimeUnit.Decade => UInt(ldt.getYear % 10)
          case TimeUnit.Unknown | TimeUnit.Forever => UInt(ldt.getYear)
          case _ => UInt.unknownInt
        }
      case TimeUnit.HalfY =>
        up match {
          case TimeUnit.Year | TimeUnit.Unknown => UInt (ldt.getMonthValue + 5) / 6
          case _ => UInt.unknownInt
        }
      case TimeUnit.Season =>
        up match {
          case TimeUnit.Year | TimeUnit.Unknown => if (season == Season.Unknown) UInt.unknownInt else UInt(season.id)
          case _ => UInt.unknownInt
        }
      case TimeUnit.QuarterY =>
        up match {
          case TimeUnit.HalfY => UInt((ldt.getMonthValue + 2) / 3 + 1) / 2
          case TimeUnit.Unknown | TimeUnit.Year => UInt(ldt.getMonthValue + 2) / 3
          case _ => UInt.unknownInt
        }
      case TimeUnit.Month =>
        up match {
          case TimeUnit.HalfY => UInt(ldt.getMonthValue - 1) % 6 + 1
          case TimeUnit.QuarterY => UInt(ldt.getMonthValue - 1) % 3 + 1
          case TimeUnit.Unknown | TimeUnit.Year => UInt(ldt.getMonthValue)
          case _ => UInt.unknownInt
        }
      case TimeUnit.Week =>
        up match {
          case TimeUnit.Month => UInt(calcWeekOfMonth(ldt))
          case TimeUnit.Unknown | TimeUnit.Year => UInt(calcWeekOfYear(ldt))
          case _ => UInt.unknownInt
        }
      case TimeUnit.Day =>
        up match {
          case TimeUnit.Week => UInt(ldt.getDayOfWeek.getValue)
          case TimeUnit.Unknown | TimeUnit.Year => UInt(ldt.getDayOfYear)
          case TimeUnit.Month => UInt(ldt.getDayOfMonth)
          case _ => UInt.unknownInt
        }
      case TimeUnit.HalfD =>
        up match {
          case TimeUnit.Unknown | TimeUnit.Day => UInt(ldt.getHour - 1) / 12 + 1
          case _ => UInt.unknownInt
        }
      case TimeUnit.POD =>
        up match {
          case TimeUnit.Unknown | TimeUnit.Day => if (TOD == TimeOfDay.Unknown) UInt.unknownInt else UInt(TOD.id)
          case _ => UInt.unknownInt
        }
      case TimeUnit.Hour =>
        up match {
          case TimeUnit.Unknown | TimeUnit.Day => UInt(ldt.getHour)
          case TimeUnit.HalfD => UInt(ldt.getHour) % 12
          case _ => UInt.unknownInt
        }
      case TimeUnit.Minute =>
        up match {
          case TimeUnit.Unknown | TimeUnit.Hour => UInt(ldt.getMinute)
          case _ => UInt.unknownInt
        }
      case TimeUnit.Second => UInt(ldt.getSecond)
        up match {
          case TimeUnit.Unknown | TimeUnit.Minute => UInt(ldt.getSecond)
          case _ => UInt.unknownInt
        }
    }
  }

  /**
   * 再给定上位单位的前提下查询值
   * @param g 查询单位
   * @param up 上位单位，为Unknown时返回缺省单位
   * @return 为空时表示无法取得，为Unknown时表示理论上支持，但无法得出确定值（也许是因为方法不完备，也许是因为确实不能确定）
   */
  def get(g: TimeUnit, up: TimeUnit): Option[UInt] = {
    def grainCheck(g: TimeUnit) = {
      (TimeUnit.isDate(g) && DateForm.hasGrain(dateForm, g)) || (TimeUnit.isTime(g) && TimeForm.hasGrain(timeForm, g))
    }
    if (!grainCheck(g)) return Option.empty
    if (up != TimeUnit.Unknown && (up >= g || !grainCheck(up))) return Option.empty

    if (unknownGrain.contains(g))
      return Some(UInt.unknownInt)
    val lValue = get(lBound, g, up)
    val rValue = get(rBound, g, up)

    if (lValue.isUnknown)
      return Some(UInt.unknownInt)

    if (lValue == rValue)
      Some(lValue)
    else if (g == TimeUnit.Day && up == TimeUnit.Week && lValue == 6 && rValue == 7)
      Some(UInt(DayOfWeek.Weekend.id))
    else
      Some(UInt.unknownInt)
  }

  /**
   * 将offset值换算到下位单位
   * @param offset 待换算值
   * @param g 下位单位
   * @param up 原单位
   * @return 换算结果，为Unknown时大概率是方法尚未支持，也可能是确实不可知
   */
  def transfer(offset: Int, g: TimeUnit, up: TimeUnit): UInt = {
    if (offset == 0)
      return UInt(0)
    if (g < up)
      return UInt.unknownInt
    if (g == up)
      return UInt(offset)
    (g, up) match {
      case (TimeUnit.Century, TimeUnit.Millennium) => UInt(offset * 10)
      case (TimeUnit.Decade, TimeUnit.Century) => UInt(offset * 10)
      case (TimeUnit.Decade, _) => transfer(offset, TimeUnit.Century, up) * 10
      case (TimeUnit.Year, TimeUnit.Decade) => UInt(offset * 10)
      case (TimeUnit.Year, _) => transfer(offset, TimeUnit.Decade, up) * 10
      case (TimeUnit.HalfY, TimeUnit.Year) => UInt(offset * 2)
      case (TimeUnit.HalfY, _) => transfer(offset, TimeUnit.Year, up) * 2
      case (TimeUnit.QuarterY, TimeUnit.HalfY) => UInt(offset * 2)
      case (TimeUnit.QuarterY, _) => transfer(offset, TimeUnit.HalfY, up) * 2
      case (TimeUnit.Month, TimeUnit.QuarterY) => UInt(offset * 3)
      case (TimeUnit.Month, _) => transfer(offset, TimeUnit.QuarterY, up) * 3
      case (TimeUnit.Week, TimeUnit.Year) => UInt.unknownInt//TODO
      case (TimeUnit.Week, TimeUnit.Month) => UInt.unknownInt//TODO
      case (TimeUnit.Day, TimeUnit.Week) => UInt(offset * 7)
      case (TimeUnit.Day, TimeUnit.Month) => UInt.unknownInt//TODO
      case (TimeUnit.Day, TimeUnit.Year) => UInt.unknownInt//TODO
      case (TimeUnit.HalfD, TimeUnit.Day) => UInt(offset * 2)
      case (TimeUnit.HalfD, _) => transfer(offset, TimeUnit.Day, up) * 2
      case (TimeUnit.Hour, TimeUnit.HalfD) => UInt(offset * 12)
      case (TimeUnit.Hour, _) => transfer(offset, TimeUnit.HalfD, up) * 12
      case (TimeUnit.Minute, TimeUnit.Hour) => UInt(offset * 60)
      case (TimeUnit.Minute, _) => transfer(offset, TimeUnit.Hour, up) * 60
      case (TimeUnit.Second, TimeUnit.Minute) => UInt(offset * 60)
      case (TimeUnit.Second, _) => transfer(offset, TimeUnit.Minute, up) * 60
      case _ => UInt.unknownInt
    }
  }

  private def getTimex3Year: String = {
    val mSym = if (unknownGrain.contains(TimeUnit.Millennium)) "X" else get(TimeUnit.Millennium, TimeUnit.Unknown).get
    val cSym = if (unknownGrain.contains(TimeUnit.Century)) "X" else get(TimeUnit.Century, TimeUnit.Millennium).get - 1
    val dSym = if (unknownGrain.contains(TimeUnit.Decade)) "X" else get(TimeUnit.Decade, TimeUnit.Century).get
    val ySym = if (unknownGrain.contains(TimeUnit.Year)) "X" else get(TimeUnit.Year, TimeUnit.Decade).get
    smallestGrain match {
      case TimeUnit.Millennium => s"$mSym"
      case TimeUnit.Century => s"$mSym$cSym"
      case TimeUnit.Decade => s"$mSym$cSym$dSym"
      case _ => s"$mSym$cSym$dSym$ySym"
    }
  }

  private def getTimex3HQ = {
    dateForm match {
      case YH => s"H${if (unknownGrain.contains(TimeUnit.HalfY)) "X" else get(TimeUnit.HalfY, TimeUnit.Year).get}"
      case YQ => s"Q${if (unknownGrain.contains(TimeUnit.QuarterY)) "X" else get(TimeUnit.QuarterY, TimeUnit.Year).get}"
    }
  }

  private def getTimex3Season = {
    TIMEX3Parser.season2string(season)
  }

  private def getTimex3Month = {
    if (unknownGrain.contains(TimeUnit.Month)) "XX" else get(TimeUnit.Month, TimeUnit.Year).get.toString(2)
  }

  private def getTimex3Week = {
    if (unknownGrain.contains(TimeUnit.Month)) "XX" else get(TimeUnit.Week, TimeUnit.Year).get.toString(2)
  }

  private def getTimex3Day = {
    if (unknownGrain.contains(TimeUnit.Day)) "XX" else get(TimeUnit.Day, TimeUnit.Month).get.toString(2)
  }

  private def getTimex3UDay = {
    val v = get(TimeUnit.Day, TimeUnit.Week).get
    if (unknownGrain.contains(TimeUnit.Day)) "XX" else if (v == DayOfWeek.Weekend.id) "WE" else v.toString(1)
  }

  private def getTimex3TOD = {
    TIMEX3Parser.tod2string(TOD)
  }

  private def getTimex3Hour = {
    if (unknownGrain.contains(TimeUnit.Hour)) "XX" else get(TimeUnit.Hour, TimeUnit.Day).get.toString(2)
  }

  private def getTimex3Minute = {
    if (unknownGrain.contains(TimeUnit.Minute)) "XX" else get(TimeUnit.Minute, TimeUnit.Hour).get.toString(2)
  }

  private def getTimex3Second = {
    if (unknownGrain.contains(TimeUnit.Second)) "XX" else get(TimeUnit.Second, TimeUnit.Minute).get.toString(2)
  }

  def toTimex3Fmt: String = {
    val dStr = if (dateForm != DateForm.Empty) {
      dateForm match {
        case Y => getTimex3Year
        case YH | YQ => s"$getTimex3Year-$getTimex3HQ"
        case YS => s"$getTimex3Year-$getTimex3Season"
        case YM => s"$getTimex3Year-$getTimex3Month"
        case YW => s"$getTimex3Year-W$getTimex3Week"
        case YMD | YD | YMWU | YWU =>
          val s1 = s"$getTimex3Year-$getTimex3Month-$getTimex3Day"
          val s2 = s"$getTimex3Year-W$getTimex3Week-$getTimex3UDay"
          if (!s1.contains("X")) s1
          else if (dateForm == YWU) s2
          else s1
      }
    } else {
      "XXXX"
    }
    val tStr = if (timeForm != TimeForm.Empty) {
      timeForm match {
        case H => s"T$getTimex3Hour"
        case HM => s"T$getTimex3Hour:$getTimex3Minute"
        case HMS => s"T$getTimex3Hour:$getTimex3Minute:$getTimex3Second"
        case DT => s"T$getTimex3TOD"
      }
    } else {
      "TXX"
    }
    (dateForm, timeForm) match {
      case (_, TimeForm.Empty) => dStr
      case (DateForm.Empty, _) => tStr
      case (_, _) => s"$dStr$tStr"
    }
  }

  def toJson =
    ("type" -> ("dateForm" -> dateForm.toString) ~ ("timeForm" -> timeForm.toString)) ~
    ("timex3fmt" -> toTimex3Fmt) ~ ("isSet" -> isSet) ~ ("smallest" -> smallestGrain.toString)

  override def toString: String = {
    compact(render(toJson))
  }

}

object TimePointImpl {
  def makeYP(year: Year): FuzzyDate = {
    val fd = new TimePointImpl()
    val v = year.value
    if (v.isKnown) {
      year.grain match {
        case TimeUnit.Millennium =>
          fd.setMillennium(v.value)
        case TimeUnit.Century =>
          fd.setCentury(v.value % 10 + 1, TimeUnit.Forever)
          fd.setMillennium(v.value / 10)
        case TimeUnit.Decade =>
          fd.setDecade(v.value, TimeUnit.Forever)
        case /*Year*/_ =>
          fd.setYear(v.value, TimeUnit.Unknown)
      }
      fd.dateForm = DateForm.Y
    }
    else {
      fd.unknownGrain += year.grain
    }
    fd.smallestGrain = year.grain
    fd
  }

  def makeY(year: UInt): FuzzyDate = {
    val fd = new TimePointImpl()
    if (year.isKnown) {
      fd.setYear(year.value, TimeUnit.Year)
    } else {
      fd.unknownGrain += TimeUnit.Year
    }
    fd.dateForm = DateForm.Y
    fd.smallestGrain = TimeUnit.Year
    fd
  }
  def makeYM(year: UInt, month: UInt): FuzzyDate = {
    val fd = makeY(year).asInstanceOf[TimePointImpl]
    if (month.isKnown) {
      fd.setMonth(month.value, TimeUnit.Year)
    } else {
      fd.unknownGrain += TimeUnit.Month
    }
    fd.dateForm = DateForm.YM
    fd.smallestGrain = TimeUnit.Month
    fd
  }

  def makeYMD(year: UInt, month: UInt, day: UInt): FuzzyDate = {
    val fd = makeYM(year, month).asInstanceOf[TimePointImpl]
    if (day.isKnown) {
      fd.setDay(day.value, TimeUnit.Month)
    } else {
      fd.unknownGrain += TimeUnit.Day
    }
    fd.dateForm = DateForm.YMD
    fd.smallestGrain = TimeUnit.Day
    fd
  }

  /**
   * Not aligned, do not follow the ISO standard. FUCK THE WEEK_YEAR
   * 不对齐，不按照ISO标准来
   * Start from 1
   * 从1开始计数
   *
   * @param year 标准年，与week无关
   * @param week 包含不完整的星期，以实际年中出现的情况为准
   */
  def makeYW(year: UInt, week: UInt): FuzzyDate = {
    val fd = makeY(year).asInstanceOf[TimePointImpl]
    if (week.isKnown) {
      fd.setWeek(week.value, TimeUnit.Year)
    } else {
      fd.unknownGrain += TimeUnit.Week
    }
    fd.dateForm = DateForm.YW
    fd.smallestGrain = TimeUnit.Week
    fd
  }

  /**
   * Not aligned, do not follow the ISO standard. FUCK THE WEEK_YEAR
   * 不对齐，不按照ISO标准来
   * Start from 1
   * 从1开始计数
   *
   * @param year 标准年，与week无关
   * @param week 包含不完整的星期，以实际年中出现的情况为准
   */
  def makeYWU(year: UInt, week: UInt, day: UInt): FuzzyDate = {
    val fd = makeYW(year, week).asInstanceOf[TimePointImpl]
    if (day.isKnown) {
      fd.setDay(day.value, TimeUnit.Week)
    } else {
      fd.unknownGrain += TimeUnit.Day
    }
    fd.dateForm = DateForm.YWU
    fd.smallestGrain = TimeUnit.Day
    fd
  }

  def makeYH(year: UInt, half: UInt): FuzzyDate = {
    val fd = makeY(year).asInstanceOf[TimePointImpl]
    if (half.isKnown) {
      fd.setHalfY(half.value)
    } else {
      fd.unknownGrain += TimeUnit.HalfY
    }
    fd.dateForm = DateForm.YH
    fd.smallestGrain = TimeUnit.HalfY
    fd
  }

  def makeYQ(year: UInt, quarter: UInt): FuzzyDate = {
    val fd = makeY(year).asInstanceOf[TimePointImpl]
    if (quarter.isKnown) {
      fd.setQuarterY(quarter.value, TimeUnit.Year)
    } else {
      fd.unknownGrain += TimeUnit.QuarterY
    }
    fd.dateForm = DateForm.YQ
    fd.smallestGrain = TimeUnit.QuarterY
    fd
  }

  def makeYS(year: UInt, season: Season): FuzzyDate = {
    val fd = makeY(year)
    fd.season = season
    fd.dateForm = DateForm.YS
    fd.smallestGrain = TimeUnit.Season
    fd
  }

  def makeDT(tod: TimeOfDay): FuzzyTime = {
    val ft = new TimePointImpl()
    ft.timeForm = TimeForm.DT
    ft.TOD = tod
    ft.smallestGrain = TimeUnit.POD
    ft
  }

  def makeH(hour: UInt): FuzzyTime = {
    val ft = new TimePointImpl()
    if (hour.isKnown) {
      ft.setHour(hour.value)
    } else {
      ft.unknownGrain += TimeUnit.Hour
    }
    ft.timeForm = TimeForm.H
    ft.smallestGrain = TimeUnit.Hour
    ft
  }

  def makeHM(hour: UInt, minute: UInt): FuzzyTime = {
    val ft = makeH(hour).asInstanceOf[TimePointImpl]
    if (minute.isKnown) {
      ft.setMinute(minute.value)
    } else {
      ft.unknownGrain += TimeUnit.Minute
    }
    ft.timeForm = TimeForm.HM
    ft.smallestGrain = TimeUnit.Minute
    ft
  }

  def makeHMS(hour: UInt, minute: UInt, second: UInt): FuzzyTime = {
    val ft = makeHM(hour, minute).asInstanceOf[TimePointImpl]
    if (second.isKnown) {
      ft.setSecond(second.value)
    } else {
      ft.unknownGrain += TimeUnit.Second
    }
    ft.timeForm = TimeForm.HMS
    ft.smallestGrain = TimeUnit.Second
    ft
  }

  def makeDateTime(date: FuzzyDate, time: FuzzyTime): TimePointImpl = {
    val fdt = new TimePointImpl()
    fdt.lBound = LocalDateTime.of(date.lBound.toLocalDate, time.lBound.toLocalTime)
    fdt.rBound = LocalDateTime.of(date.rBound.toLocalDate, time.rBound.toLocalTime)
    fdt.smallestGrain = if (time.smallestGrain == TimeUnit.Unknown) date.smallestGrain else time.smallestGrain
    fdt.season = date.season
    fdt.TOD = time.TOD
    fdt.dateForm = date.dateForm
    fdt.timeForm = time.timeForm
    fdt.unknownGrain = date.unknownGrain ++ time.unknownGrain
    fdt
  }

  def makeUnknown: TimePointImpl = {
    val fdt = new TimePointImpl()
    fdt.unknownGrain = Set((TimeUnit.values - TimeUnit.Unknown).toSeq: _*)
    fdt
  }
}
