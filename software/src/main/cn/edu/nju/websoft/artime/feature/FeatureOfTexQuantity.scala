package cn.edu.nju.websoft.artime.feature

import cn.edu.nju.websoft.artime.struct.TExpression

case class FeatureOfTexQuantity(
  value: Seq[String]
) extends StructuredFeature {
  type W = Seq[String]
  override def factory: FeatureFactory = FeatureOfTexQuantity

  override def toDoubles: Array[Double] = {
    val vars = FeatureOfRuleVar.getVarIndex
    val result = Array.ofDim[Double](vars.size + 1)
    value.foreach(v => {
      val id = vars.get(v)
      if (id.nonEmpty)
        result(id.get) += 1.0
      else
        result(vars.size) += 1.0
    })
    result
  }
}

object FeatureOfTexQuantity extends NominalFeatureFactory {
  val name: String = "variable types"
  val featureType: String = "counter"
  type T = TExpression

  override def makeValueForUpdate(x: T): (FeatureOfTexQuantity, IndexedSeq[String]) = {
    val result = x.tokens.map(_.qType).collect { case Some(q) => q }
    (FeatureOfTexQuantity(result), result)
  }
}

