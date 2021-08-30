package cn.edu.nju.websoft.artime.feature

import cn.edu.nju.websoft.artime.struct.TExpression

case class FeatureOfTense (
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

object FeatureOfTense extends NominalFeatureFactory {
  type T = TExpression
  val name: String = "verb tense"
  val featureType: String = "nominal"

  def makeValueForUpdate(tex: TExpression): (FeatureOfTense, Seq[String]) = {
    val sentence = tex.getContextWithStart._1
    val nearestVerbTense = (0 until tex.tokenBegin).reverse.collectFirst {
      case i: Int if sentence.tokens(i).tag.startsWith("V") =>
        sentence.tokens(i).tag match {
          case s if s.endsWith("D") =>
            "PAST"
          case _ =>
            if (i > 0 && sentence.tokens(i-1).tag == "MD")
              "FUTURE"
            else
              "PRESENT"
        }
    }
    (FeatureOfTense(nearestVerbTense.getOrElse(" ")), nearestVerbTense.toSeq)
  }
}



