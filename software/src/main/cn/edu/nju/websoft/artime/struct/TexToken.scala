package cn.edu.nju.websoft.artime.struct

import cn.edu.nju.websoft.artime.utils.ReverseNodeIndex
import cn.edu.nju.websoft.artime.action.{Action, ModifyValAction}
import cn.edu.nju.websoft.artime.rule.Rule
import cn.edu.nju.websoft.artime.semantic.constant
import cn.edu.nju.websoft.artime.semantic.constant.TimeUnit
import cn.edu.nju.websoft.artime.semantic.constant.TimeUnit.TimeUnit
import cn.edu.nju.websoft.artime.utils.{NumChecker, ReverseNodeIndex, UInt}
import edu.stanford.nlp.simple.Token
import weka.core.pmml.jaxbbindings.False

import scala.util.matching.Regex

case class TexToken(
  var text: String,
  var lemma: String,
//  var parameters: IndexedSeq[String],
  var  tag: String,
  var charBegin: Int,
  var charEnd: Int,
  var indexInSentence: Option[ReverseNodeIndex[TSentence]]
) {
  var qType: Option[String] =
    if (TexToken.qTagSet.contains(tag)) Some(tag)
    else Option.empty
  def stdStr =
    if (qType.nonEmpty) s"$lemma/${qType.get}"
    else if(pType.nonEmpty) s"$lemma/${pType.get}"

    else s"$lemma/$tag"

  //匹配
  var pType: Option[String] =
    if (TexToken.pTagSet.contains(tag)) Some(tag)
    else Option.empty


  def parseAsDateTime: List[((TimeUnit, TimeUnit), Action)] = {
    val dateConstant: Seq[(Regex, constant.TimeUnit.Value)] =
      Seq(("\\[Y:(\\d+)\\]".r, TimeUnit.Year), ("\\[M:(\\d+)\\]".r, TimeUnit.Month), ("\\[D:(\\d+)\\]".r, TimeUnit.Day))
    val timeConstant: Seq[(Regex, constant.TimeUnit.Value)] =
      Seq(("\\[H:(\\d+)\\]".r, TimeUnit.Hour), ("\\[M:(\\d+)\\]".r, TimeUnit.Minute), ("\\[S:(\\d+)\\]".r, TimeUnit.Second))
    def getModify(re: Regex, grain: TimeUnit) = {
      re.findFirstMatchIn(lemma).map(y => {
//        println(y.groupCount, y.group(0))
        (
          (grain, TimeUnit.defaultUpperGrain(grain)),
          ModifyValAction(UInt(y.group(1).toInt), grain, Option.empty)
        )
      })
    }
    qType match {
      case Some("DATE") =>
        dateConstant.flatMap(x => getModify(x._1, x._2)).toList
      case Some("TIME") =>
        timeConstant.flatMap(x => getModify(x._1, x._2)).toList
      case _ =>
        List.empty
    }
  }

  def getQuantity: String = {
    NumChecker.collectNumber(lemma).getOrElse({
      val table = Rule.matchTable.values.find(_.keys.exists(k => lemma.matches(k))).getOrElse(Map.empty)
      val key = table.keys.find(k => lemma.matches(k)).getOrElse("None")
      table.getOrElse(key, ":None").dropWhile(_ != ':').drop(1)
    }).toString
  }


  override def toString: String = s"$text[$stdStr]"
}

object TexToken {
  val On=false
  val qTagSet: Set[String] = Set("NUM", "ORD", "YEAR", "DECADE", "DATE", "TIME")
  val pTagSet: Set[String] = if(On) Set(
    "LAST",
    "THIS",
    "NEXT",
    "PAST",
    "NOW",
    "PART_WORD",
    "AM",
    "PM",
    "INEQ"
  )
  else Set("INEQ")
  def apply(token: Token): TexToken = {
    this(token.originalText(), token.lemma(), token.posTag(), token.characterOffsetBegin(), token.characterOffsetEnd(), Option.empty)
  }
}