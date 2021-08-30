package cn.edu.nju.websoft.artime.action

trait Action {
  def getParameterStrings: List[String]
  def priority: Int
}
