package cn.edu.nju.websoft.artime.feature

import cn.edu.nju.websoft.artime.rule.{TokenPattern, VariableTokenPattern}

case class FeatureOfRuleVar(
  value: Seq[String]
) extends StructuredFeature {
  type W = Seq[String]
  override def factory: FeatureFactory = FeatureOfRuleVar

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

object FeatureOfRuleVar extends NominalFeatureFactory {
  override val name: String = "variable types in rule"
  override val featureType: String = "counter"
  type T = IndexedSeq[TokenPattern]

  val varRE = "(.+):$(\\d+)".r
  override def makeValueForUpdate(x: T): (FeatureOfRuleVar, IndexedSeq[String]) = {
    val result = x.collect { case x if x.isVariable => x.asInstanceOf[VariableTokenPattern].qType }
    (FeatureOfRuleVar(result), result)
  }
}


