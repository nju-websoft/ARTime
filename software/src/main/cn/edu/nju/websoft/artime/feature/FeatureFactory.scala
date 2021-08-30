package cn.edu.nju.websoft.artime.feature

import cn.edu.nju.websoft.artime.struct.TExpression

trait FeatureFactory {
  type T <: Any
  val name: String
  val featureType: String
  def makeValue(x: T): StructuredFeature
}