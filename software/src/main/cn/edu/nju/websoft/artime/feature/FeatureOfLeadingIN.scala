package cn.edu.nju.websoft.artime.feature

import cn.edu.nju.websoft.artime.struct.TExpression

import scala.collection.mutable

case class FeatureOfLeadingIN (
  value: String
) extends StructuredFeature {
  type W = String
  override def factory: FeatureFactory = FeatureOfLeadingIN

  override def toDoubles: Array[Double] = {
    val vars = FeatureOfLeadingIN.getVarIndex
    val result = Array.ofDim[Double](vars.size + 1)
    val id = vars.get(value)
    if (id.nonEmpty)
      result(id.get) += 1.0
    else
      result(vars.size) += 1.0
    result
  }
}

object FeatureOfLeadingIN extends NominalFeatureFactory {
  type T = TExpression
  val name: String = "leading preposition"
  val featureType: String = "string"
  def makeValueForUpdate(x: T): (FeatureOfLeadingIN, Seq[String]) = {
    val sentence = x.getContextWithStart._1
    val nearestIN = (0 to Math.min(x.tokenBegin, sentence.tokens.length - 1)).reverse.collectFirst {
      case i: Int if sentence.tokens(i).tag.matches("IN|TO") => sentence.tokens(i).lemma
    }
    (FeatureOfLeadingIN(nearestIN.getOrElse(" ")), nearestIN.toSeq)
  }
}


