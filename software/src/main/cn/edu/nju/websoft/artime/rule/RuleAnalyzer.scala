package cn.edu.nju.websoft.artime.rule

import cn.edu.nju.websoft.artime.feature.FeaturedInterpretation

import scala.collection.JavaConverters._

object RuleAnalyzer {
  def getStatistics(interpretations: Seq[FeaturedInterpretation]) = {
    val ruleUnsorted = interpretations.map(_.rule).distinct
    val patternStatistics = interpretations
      .map(fi => (fi.rule.texPattern.mkString(" "), fi.texID))
      .distinct.groupBy(_._1).mapValues(_.length)
    val featurelessRuleStatistics = interpretations
      .map(fi => ((fi.rule.texPattern.mkString(" "), fi.rule.valuePattern.mkString(" ")), fi.texID))
      .distinct.groupBy(_._1).mapValues(_.length)
//    val features = ruleUnsorted.flatMap(_.feature)
//    val featureRuleStatistics = features.flatMap(f => interpretations
//      .withFilter(fi => fi.features.contains(f))
//      .map(fi => ((fi.rule.texPattern.mkString(" "), fi.rule.valuePattern.mkString(" "), f), fi.texID))
//      .distinct.groupBy(_._1).mapValues(_.length)).toMap

    def getScore(r: Rule, f: Option[String]) = {
      val pScore = patternStatistics(r.texPattern.mkString(" "))
      val rScore =
//        if (f.isEmpty)
          featurelessRuleStatistics((r.texPattern.mkString(" "), r.valuePattern.mkString(" ")))
//        else
//          featureRuleStatistics.getOrElse((r.texPattern.mkString(" "), r.valuePattern.mkString(" "), f.get), 0)
      val lScore = r.valuePattern.count(_.startsWith("Action:"))
      val vScore = r.texPattern.count(_.isVariable)

      (-pScore, -rScore, -vScore / (lScore + 1e-6))
    }

    ruleUnsorted/*.flatMap(_.splitByFeature)*/.distinct.map(r => {
//      val f = r.feature.headOption
      r -> getScore(r, Option.empty)
    }).sortBy(_._2)
  }


  def collectSubRule(rules: IndexedSeq[Rule]) = {
    rules.foreach(rule => {
      val actions = rule.valuePattern
        .foldRight((List.empty[List[String]], List.empty[String]))((p, record) => {
          val buffer = p :: record._2
          if (p.startsWith("Action")) {
            (buffer :: record._1, List.empty[String])
          } else {
            (record._1, buffer)
          }
        })._1
      //        val result = mutable.ArrayBuilder.make()[(Seq[TokenPattern], Seq[List[String]])]
      //        rule.texPattern.foreach(tp => {
      //          actions.foreach(a => {
      //            if (tp.isVariable) {
      //              val varString = s":$$${tp.asInstanceOf[VariableTokenPattern].varID}"
      //              if (a.exists(p => p.contains(varString))) {
      //                result += ((Seq(tp), Seq(a)))
      //              }
      //            }
      //          })
      //        })

    })
  }

}
