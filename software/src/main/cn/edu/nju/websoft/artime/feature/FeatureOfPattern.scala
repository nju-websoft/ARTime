package cn.edu.nju.websoft.artime.feature

import cn.edu.nju.websoft.artime.feature
import cn.edu.nju.websoft.artime.rule.{QuantityTokenPattern, TokenPattern}

import scala.util.matching.Regex

case class FeatureOfPattern(
  value: String
) extends StructuredFeature {
  type W = String
  override def factory: FeatureFactory = FeatureOfPattern

  override def toDoubles: Array[Double] = {
    val vars = FeatureOfPattern.getVarIndex
    val result = Array.ofDim[Double](vars.size + 1)
    val id = vars.get(value)
    if (id.nonEmpty)
      result(id.get) += 1.0
    else
      result(vars.size) += 1.0
    result
  }
}

object FeatureOfPattern extends NominalFeatureFactory {
  override type T = IndexedSeq[TokenPattern]
  override val name: String = "tex pattern"
  override val featureType: String = "string"

  val qRE: Regex = "(.+):(.+)".r

  def makeValueForUpdate(x: T): (FeatureOfPattern, Seq[String]) = {
    val pattern = x.map {
      case x if x.isQuantity => x.asInstanceOf[QuantityTokenPattern].qType
      case x => x
    }.mkString("\t")
    (FeatureOfPattern(pattern), Seq(pattern))
  }
}
