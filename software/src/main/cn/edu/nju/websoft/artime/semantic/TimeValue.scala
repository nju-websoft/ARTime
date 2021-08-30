package cn.edu.nju.websoft.artime.semantic

import org.json4s.JsonAST
import org.json4s.native.Json

trait TimeValue {
  var tid : Int
  var timex3type : String
  var isSet: Boolean
  def toJson: JsonAST.JValue
  def toTimex3Fmt: String
}
