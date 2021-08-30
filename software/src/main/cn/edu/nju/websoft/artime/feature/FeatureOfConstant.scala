package cn.edu.nju.websoft.artime.feature

import cn.edu.nju.websoft.artime.rule.{QuantityTokenPattern, TokenPattern}

import scala.collection.mutable

case class FeatureOfConstant(
  value: Seq[String]
) extends StructuredFeature {
  type W = Seq[String]
  override def factory: FeatureFactory = FeatureOfConstant

  override def toDoubles: Array[Double] = {
    val vars = FeatureOfLeadingIN.getVarIndex
    val result = Array.ofDim[Double](vars.size)
    value.foreach(v => if (vars.contains(v)) {
      result(vars(v)) += 1.0
    })
    result
  }
}

object FeatureOfConstant extends NominalFeatureFactory {
  type T = IndexedSeq[TokenPattern]
  val name: String = "number of constant (non-param quantities) "
  val featureType: String = "count"

  val constantRE = "(.+):([^$]+)".r
  def makeValueForUpdate(x: T): (FeatureOfConstant, IndexedSeq[String]) = {
    val result = x.collect { case x if x.isQuantity && !x.isVariable => x.asInstanceOf[QuantityTokenPattern].qType }
    (FeatureOfConstant(result), result)
  }
}
