package cn.edu.nju.websoft.artime.io

import cn.edu.nju.websoft.artime.semantic.constant.DayOfWeek.DayOfWeek
import cn.edu.nju.websoft.artime.semantic.constant.Direction.Direction
import cn.edu.nju.websoft.artime.semantic.constant.Season._
import cn.edu.nju.websoft.artime.semantic.constant.TimeOfDay.TimeOfDay
import cn.edu.nju.websoft.artime.semantic.constant.TimeUnit.TimeUnit
import cn.edu.nju.websoft.artime.semantic.constant.{DayOfWeek, Direction, TimeOfDay, TimeUnit}
import cn.edu.nju.websoft.artime.semantic._
import cn.edu.nju.websoft.artime.utils.UInt
import com.codecommit.gll.{Failure, FailureData, RegexParsers, Success}
import com.typesafe.scalalogging.Logger
import shapeless.poly.->
import shapeless.syntax.std.tuple._

import scala.xml.{Elem, XML}

object TIMEX3Parser extends RegexParsers{
  val logger = Logger(this.getClass)

  def year_prefix: Parser[String] = "AD|BC".r ^^ { p =>
    logger.debug("YEAR_PREFIX")
    p.toString
  }

  def year: Parser[Year] = opt(year_prefix) ~ "\\d{1,4}|XXXX".r  ^^ { (p, y) =>
    logger.debug("YEAR")
    val negative = p.exists(_.equals("AD"))
    y match {
      case "XXXX" => Year(UInt.unknownInt, TimeUnit.Year)
      case v => {
        val value = UInt(if (negative) -v.toInt else v.toInt)
        val mapping = Map(1 -> TimeUnit.Millennium, 2 -> TimeUnit.Century, 3 -> TimeUnit.Decade, 4 -> TimeUnit.Year)
        val grain = mapping(v.length)
        Year(value, grain)
      }
    }
  }

  def season: Parser[Season] = "SP|SU|FA|WI".r ^^ { s =>
    logger.debug("SEASON")
    s match {
      case "SP" => Spring
      case "SU" => Summer
      case "FA" => Fall
      case "WI" => Winter
    }
  }

  def month: Parser[UInt] = "0[1-9]|1[0-2]|XX".r ^^ { m =>
    logger.debug("MONTH")
    makeUncertainNumber(m)
  }

  def day_in_week: Parser[UInt] = "[1-7]".r ^^ { u =>
    logger.debug("DAY_IN_WEEK")
    makeUncertainNumber(u)
  }

  def part_of_week: Parser[DayOfWeek] = "WE".r ^^ { pw =>
    logger.debug("PART_OF_WEEK")
    pw match {
      case "WE" => DayOfWeek.Weekend
    }
  }

  def day: Parser[UInt] = "0[1-9]|[1-2][0-9]|3[0-1]|XX".r ^^ { d =>
    logger.debug("DAY")
    makeUncertainNumber(d)
  }

  def part_of_day: Parser[TimeOfDay] = "MO|MI|AF|EV|NI|PM|DT".r ^^ { pd =>
    logger.debug("PART_OF_DAY")
    pd match {
      case "MO" => TimeOfDay.Morning
      case "MI" => TimeOfDay.MidDay
      case "AF" => TimeOfDay.Afternoon
      case "EV" => TimeOfDay.Evening
      case "NI" => TimeOfDay.Night
      case "PM" => TimeOfDay.PM
      case "DT" => TimeOfDay.DayTime
    }
  }

  def hour: Parser[UInt] = "[01][0-9]|2[0-4]|XX".r ^^ { h =>
    logger.debug("HOUR")
    if (h.charAt(0) == 'X')
      UInt.unknownInt
    else
      UInt(h.toInt)
  }
  def minute: Parser[UInt] = "[0-5][0-9]|XX".r ^^ { m =>
    logger.debug("MINUTE")
    if (m.charAt(0) == 'X')
      UInt.unknownInt
    else
      UInt(m.toInt)
  }
  def second: Parser[UInt] = "[0-5][0-9]|60|XX".r ^^ { s =>
    logger.debug("SECOND")
    if (s.charAt(0) == 'X')
      UInt.unknownInt
    else
      UInt(s.toInt)
  }

  def number: Parser[UInt] = "\\d+|X+".r ^^ { n =>
    logger.debug("NUMBER")
    makeUncertainNumber(n)
  }

  def makeUncertainNumber(s: String): UInt = {
    s.charAt(0) match {
      case 'X' => UInt.unknownInt
      case _ => new UInt(s.toInt)
    }
  }

  def date_grain: Parser[TimeUnit] = "ML|CE|DE|[LCEYQMWD]".r ^^ { g =>
    logger.debug("GRAIN")
    g match {
      case "L" | "ML" => TimeUnit.Millennium
      case "C" | "CE" => TimeUnit.Century
      case "E" | "DE" => TimeUnit.Decade
      case "Y" => TimeUnit.Year
      case "Q" => TimeUnit.QuarterY
      case "M" => TimeUnit.Month
      case "W" => TimeUnit.Week
      case "D" => TimeUnit.Day
    }
  }

  def time_grain: Parser[TimeUnit] = "[HMS]".r ^^ { g =>
    logger.debug("GRAIN")
    g match {
      case "H" => TimeUnit.Hour
      case "M" => TimeUnit.Minute
      case "S" => TimeUnit.Second
    }
  }

  def grain2char(g: TimeUnit): Char = {
    g match {
      case TimeUnit.Millennium => 'L'
      case TimeUnit.Century => 'C'
      case TimeUnit.Decade => 'E'
      case TimeUnit.Year => 'Y'
      case TimeUnit.HalfY => 'H'
      case TimeUnit.QuarterY => 'Q'
      case TimeUnit.Month => 'M'
      case TimeUnit.Week => 'W'
      case TimeUnit.Day => 'D'
      case TimeUnit.Hour => 'H'
      case TimeUnit.Minute => 'M'
      case TimeUnit.Second => 'S'
      case TimeUnit.Unknown => 'X'
    }
  }

  def grain2String(g: TimeUnit): String = {
    g match {
      case TimeUnit.Millennium => "LE"
      case TimeUnit.Century => "CE"
      case TimeUnit.Decade => "DE"
      case TimeUnit.Year => "Y"
      case TimeUnit.HalfY => "H"
      case TimeUnit.QuarterY => "Q"
      case TimeUnit.Month => "M"
      case TimeUnit.Week => "W"
      case TimeUnit.Day => "D"
      case TimeUnit.Hour => "H"
      case TimeUnit.Minute => "M"
      case TimeUnit.Second => "S"
      case TimeUnit.Unknown => "X"
    }
  }

  def tod2string(t: TimeOfDay): String = {
    t match {
      case TimeOfDay.Morning => "MO"
      case TimeOfDay.MidDay => "MI"
      case TimeOfDay.Afternoon => "AF"
      case TimeOfDay.Evening => "EV"
      case TimeOfDay.Night => "NI"
      case TimeOfDay.PM => "PM"
      case TimeOfDay.DayTime => "DT"
      case TimeOfDay.Unknown => "XX"
    }
  }

  def season2string(s: Season): String = {
    s match {
      case Spring => "SP"
      case Summer => "SU"
      case Fall => "FA"
      case Winter => "WI"
    }
  }

  def direction2string(d: Direction): String = {
    d match {
      case Direction.Past => "PAST_REF"
      case Direction.Present => "PRESENT_REF"
      case Direction.Future => "FUTURE_REF"
    }
  }

  def ref: Parser[TimeRef] = "(PAST|PRESENT|FUTURE)_REF".r ^^ { s =>
    logger.debug("REF")
    s.charAt(1) match {
      case 'A' => TimeRef(Direction.Past)
      case 'R' => TimeRef(Direction.Present)
      case 'U' => TimeRef(Direction.Future)
    }
  }

  def calendarDate: Parser[FuzzyDate] =
    year ~ opt("-" ~ month ~ opt("-" ~ day)) ^^ { (year: Year, opt_md) =>
      logger.debug("CALENDAR_DATE")
      if (opt_md.isEmpty) {
        TimePointImpl.makeYP(year)
      }
      else {
        val md = opt_md.get
        val month: UInt = md._1._2
        if (md._2.isEmpty) {
          TimePointImpl.makeYM(year.getYear, month)
        }
        else {
          val day: UInt = md._2.get._2
          TimePointImpl.makeYMD(year.getYear, month, day)
        }
      }
    }

  def quarterDate: Parser[FuzzyDate]
  = year ~ "-" ~ "Q[1-4]|QX|H[1-2]|HX".r ^^ { (year: Year, _, part: String) =>
      logger.debug("QUARTER_DATE")
    if (part.charAt(0) == 'Q')
      TimePointImpl.makeYQ(year.getYear, makeUncertainNumber(part.drop(1)))
    else
      TimePointImpl.makeYH(year.getYear, makeUncertainNumber(part.drop(1)))
    }

  def seasonDate: Parser[FuzzyDate]
  = year ~ "-" ~ season ^^ { (year: Year, _, season: Season) =>
    logger.debug("QUARTER_DATE")
    TimePointImpl.makeYS(year.getYear, season)
  }

  def weekDate: Parser[FuzzyDate]
  = year ~ "-" ~ "W" ~ number ~ opt("-" ~ day_in_week) ^^ { (year: Year, _, _, week: UInt, opt_dw) =>
      logger.debug("WEEK_DATE")
      if (opt_dw.isEmpty) {
        TimePointImpl.makeYW(year.getYear, week)
      }
      else {
        val day: UInt = opt_dw.get._2
        TimePointImpl.makeYWU(year.getYear, week, day)
      }
    } | year ~ "-" ~ "W" ~ number ~ "-" ~ part_of_week ^^ { (year: Year, _, _, week: UInt, _, pow: DayOfWeek) =>
      TimePointImpl.makeYWU(year.getYear, week, UInt(pow.id))
    }

  @deprecated
  def multi_year: Parser[Year] = year ~ "X{1,3}".r ^^ { (year: Year, _) =>
    logger.debug("MULTI_YEAR")
    year
  }

  def date: Parser[FuzzyDate] = calendarDate | quarterDate | seasonDate | weekDate

  def duration: Parser[TimeDuration]
  = "P" ~ rep1(number ~ date_grain) ^^ { (_, du) =>
      logger.debug("DURATION")
      new TimeDuration(du.map(dg => (dg._2, dg._1)))
    } | "P" ~ rep(number ~ date_grain) ~ "T" ~ rep1(number ~ time_grain) ^^ { (_, ddu, _, tdu) =>
      logger.debug("DURATION")
      new TimeDuration((ddu ::: tdu).map(dg => (dg._2, dg._1)))
    } | "PXX" ^^ { _ =>
      logger.debug("DURATION")
      TimeDuration.makeUnknownDuration
    }

  def time: Parser[FuzzyTime]
  = "T" ~ part_of_day ^^ { (_, pod: TimeOfDay) =>
      logger.debug("TIME")
      TimePointImpl.makeDT(pod)
    } | "T" ~ hour ~ opt(":" ~ minute ~ opt(":" ~ second)) ^^ { (_, hour, opt_ms) =>
      logger.debug("TIME")
      if (opt_ms.isEmpty) {
        TimePointImpl.makeH(hour)
      } else {
        val minute: UInt = opt_ms.get._1._2
        val opt_s = opt_ms.get._2
        if (opt_s.isEmpty) {
          TimePointImpl.makeHM(hour, minute)
        }
        else {
          val second: UInt = opt_s.get._2
          TimePointImpl.makeHMS(hour, minute, second)
        }
      }
    }

  def date_time: Parser[TimePointImpl]
    = date ~ time ^^ { (date, time) =>
      logger.debug("DATE_TIME")
      TimePointImpl.makeDateTime(date, time)
    }

  def timex3: Parser[TimeValue] = date | time | date_time | duration | ref

  /**
   * @param str
   * @return
   */
  def parseAnnotation(str: String): Either[FailureData, TimeValue] = {
    val xml = XML.loadString(s"$str</TIMEX3>")
    parseAnnotation(xml)
  }

  /**
   *
   * @param xml
   * @return
   */
  def parseAnnotation(xml: Elem): Either[FailureData, TimeValue] = {
    val t = xml \@ "type"
    val tid= xml \@ "tid"
    if ((xml \@ "value").isEmpty || (xml \@ "value").equals("na"))
      return Right(TimePointImpl.makeUnknown)
    timex3(xml \@ "value").head match {
      case Success(matched, _) => {
        if (t == "SET") matched.isSet = true
        matched.tid=tid.drop(1).toInt
        matched.timex3type=t
        Right(matched)
      }
      case Failure(msg, _) => Left(msg)
    }
  }

  def parse(str: String): Either[FailureData, TimeValue] = {
    timex3(str).head match {
      case Success(matched, _) => Right(matched)
      case Failure(msg, _) => Left(msg)
    }
  }

  def main(args: Array[String]) = {
    println(timex3("2010-W01").toList.head)
    println(timex3("2020-Q3").toList.head)
    println(timex3("2020-08-02TXX:XX").toList.head)
    println(timex3("PXX").toList.head)
    println(timex3("P10Y1M").toList.head)
    println(timex3("199X").toList.head)
    println(parseAnnotation("<TIMEX3 tid=\"t8\" type=\"SET\" value=\"P1M\">").right.get.isSet)
  }
}
