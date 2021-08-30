package cn.edu.nju.websoft.artime.feature

trait StructuredFeature {
  type W <: Any
  def value: W
  def factory: FeatureFactory
  def name: String = factory.name
  val featureType: String = factory.featureType
  def toDoubles: Array[Double]

  override def toString: String = s"$name:[$value]"
}
