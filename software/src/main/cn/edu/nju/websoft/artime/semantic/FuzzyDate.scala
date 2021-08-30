package cn.edu.nju.websoft.artime.semantic

import java.time.LocalDateTime

import cn.edu.nju.websoft.artime.semantic.constant.TimeUnit._
import cn.edu.nju.websoft.artime.semantic.constant.{DateForm, Season, TimeUnit}
import cn.edu.nju.websoft.artime.utils.UInt

import scala.collection.mutable

trait FuzzyDate extends FuzzyTimePoint {
  var lBound: LocalDateTime
  var rBound: LocalDateTime
  var smallestGrain: TimeUnit.Value
  var season: Season.Value
  var dateForm: DateForm.Value
  var unknownGrain: Set[TimeUnit.Value]
}