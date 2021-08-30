package cn.edu.nju.websoft.artime.feature

object RefType extends Enumeration {
  type RefType = Value
  val DCT = Value(0)
  val PREV = Value(1)
  val SUCC = Value(2)
  val UNKNOWN = Value(99)
}
