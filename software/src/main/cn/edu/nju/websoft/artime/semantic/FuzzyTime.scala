package cn.edu.nju.websoft.artime.semantic

import java.time.{LocalDate, LocalDateTime, LocalTime}

import cn.edu.nju.websoft.artime.semantic.constant.TimeOfDay.TimeOfDay
import cn.edu.nju.websoft.artime.semantic.constant.TimeUnit.TimeUnit
import cn.edu.nju.websoft.artime.semantic.constant.{DateForm, Season, TimeForm, TimeOfDay, TimeUnit}
import cn.edu.nju.websoft.artime.utils.UInt

import scala.collection.mutable

trait FuzzyTime extends FuzzyTimePoint {
  var lBound: LocalDateTime
  var rBound: LocalDateTime
  var smallestGrain: TimeUnit.Value
  var TOD: TimeOfDay.Value
  var timeForm: TimeForm.Value
  var unknownGrain: Set[TimeUnit.Value]
}

