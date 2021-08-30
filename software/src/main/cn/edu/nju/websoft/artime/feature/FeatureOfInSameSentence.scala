package cn.edu.nju.websoft.artime.feature

import cn.edu.nju.websoft.artime.struct.TExpression

case class FeatureOfInSameSentence (
  value: Boolean
) extends StructuredFeature {
  type W = Boolean
  override def factory: FeatureFactory = FeatureOfInSameSentence

  override def toDoubles: Array[Double] =
    if (value) Array(.0, 1.0)
    else Array(1.0, .0)
}

object FeatureOfInSameSentence extends FeatureFactory {
  type T = (TExpression, TExpression)
  val name: String = "In same sentence"
  val featureType: String = "nominal"

  def makeValue(x: (TExpression, TExpression)): FeatureOfInSameSentence = {
    val result =
      x._1.indexInSentence.isDefined && x._2.indexInSentence.isDefined &&
      x._1.indexInSentence.get.upperNode == x._2.indexInSentence.get.upperNode
    FeatureOfInSameSentence(result)
  }
}
