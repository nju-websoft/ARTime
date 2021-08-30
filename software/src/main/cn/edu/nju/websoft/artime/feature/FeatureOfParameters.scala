package cn.edu.nju.websoft.artime.feature

import cn.edu.nju.websoft.artime.action.Action
import cn.edu.nju.websoft.artime.utils.Counter

import scala.util.matching.Regex

case class FeatureOfParameters (
  value: Seq[String]
) extends StructuredFeature {
  type W = Seq[String]
  override def factory: FeatureFactory = FeatureOfParameters

  override def toDoubles: Array[Double] = {
    val vars = FeatureOfParameters.getVarIndex
    val result = Array.ofDim[Double](vars.size + 1)
    value.foreach(v => {
      val id = vars.get(v)
      if (id.nonEmpty)
        result(id.get) += 1.0
      else
        result(vars.size) += 1.0
      })
    result
  }}

object FeatureOfParameters extends NominalFeatureFactory {
  val name: String = "statistics of parameters"
  val featureType: String = "counter"
  type T = IndexedSeq[Action]

  val numValueRE: Regex = "Value:(\\d+)".r
  def makeValueForUpdate(x: T): (FeatureOfParameters, IndexedSeq[String]) = {
    val result = x.flatMap(_.getParameterStrings).map {
      case numValueRE(_) => "Value:numeric"
      case x => x
    }
    (FeatureOfParameters(result), result)
  }
}
