package cn.edu.nju.websoft.artime.feature

import cn.edu.nju.websoft.artime.action.Action
import cn.edu.nju.websoft.artime.feature.RefType.RefType
import cn.edu.nju.websoft.artime.rule.Rule
import cn.edu.nju.websoft.artime.semantic.TimeValue
import cn.edu.nju.websoft.artime.struct.TExpression
import de.bwaldvogel.liblinear.Feature

case class FeaturedInterpretation(
  texID: String,
  tex: TExpression,
  rule: Rule,
  refType: RefType,
  meaning: IndexedSeq[Action],
  result: TimeValue,
  features: Array[Feature]
) {
  override def toString: String = s"ID=$texID:Rule=$rule:Result=$result"

  override def hashCode(): Int = this.toString.hashCode

  override def equals(obj: Any): Boolean =
    obj.isInstanceOf[FeaturedInterpretation] && obj.asInstanceOf[FeaturedInterpretation].toString == this.toString
}
