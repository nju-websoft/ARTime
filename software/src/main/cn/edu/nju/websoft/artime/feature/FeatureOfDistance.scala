package cn.edu.nju.websoft.artime.feature

import cn.edu.nju.websoft.artime.struct.TExpression
import weka.core.Attribute

case class FeatureOfDistance (
  value: Double
) extends StructuredFeature {
  type W = Double
  override def factory: FeatureFactory = FeatureOfDistance
  override def toDoubles: Array[Double] = Array(value)
}

object FeatureOfDistance extends FeatureFactory {
  type T = (TExpression, TExpression)
  val name: String = "distance"
  val featureType: String = "numeric"

  override def makeValue(x: (TExpression, TExpression)): FeatureOfDistance = {
    val result =
      Math.min(
        Math.abs(x._1.tokenBegin - (x._2.tokenBegin + x._2.tokens.length)),
        Math.abs(x._2.tokenBegin - (x._1.tokenBegin + x._1.tokens.length))
      )
    FeatureOfDistance(result)
  }
}

