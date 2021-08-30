package cn.edu.nju.websoft.artime.feature

import cn.edu.nju.websoft.artime.struct.TExpression

case class FeatureOfSingularUnit(
  value: String
) extends StructuredFeature {
    type W = String
    override def factory = FeatureOfTense

    override def toDoubles: Array[Double] = {
      val vars = FeatureOfTense.getVarIndex
      val result = Array.ofDim[Double](vars.size + 1)
      val id = vars.get(value)
      if (id.nonEmpty)
        result(id.get) += 1.0
      else
        result(vars.size) += 1.0
      result
    }
  }

object FeatureOfSingularUnit extends NominalFeatureFactory {
    type T = TExpression
    val name: String = "singular unit"
    val featureType: String = "nominal"

    def makeValueForUpdate(tex: TExpression): (FeatureOfSingularUnit, Seq[String]) = {
      if (tex.tokens.exists(_.qType contains "TIME_UNIT")) {
        val isSN = tex.tokens.forall(t => !t.qType.contains("TIME_UNIT") || !t.tag.endsWith("S"))
        (FeatureOfSingularUnit(isSN.toString), Seq(isSN.toString))
      }
      else
        (FeatureOfSingularUnit(" "), Seq.empty)
    }
}


