package cn.edu.nju.websoft.artime.feature

import scala.collection.mutable

trait NominalFeatureFactory extends FeatureFactory {
  val varBuffer: mutable.Set[String] = mutable.Set.empty
  var needUpdate: Boolean = false
  var varIndex: Map[String, Int] = Map.empty
  def getVarIndex: Map[String, Int] = {
    if (needUpdate) {
      varIndex = varBuffer.toIndexedSeq.zipWithIndex.toMap
      needUpdate = false
    }
    varIndex
  }
  /**
   * @param x:
   * @return _1:
   */
  def makeValueForUpdate(x: T): (StructuredFeature, Seq[String])

  def makeValue(x: T): StructuredFeature = {
    needUpdate = true
    val result = makeValueForUpdate(x)
    result._2.foreach(varBuffer.+=)
    result._1
  }
}
