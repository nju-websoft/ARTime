package cn.edu.nju.websoft.artime.struct

import cn.edu.nju.websoft.artime.semantic.TimeValue
import cn.edu.nju.websoft.artime.utils.ReverseNodeIndex
import org.json4s.JsonDSL._
import org.json4s.native.JsonMethods._

case class TExpression (
  tokens: IndexedSeq[TexToken],
  var indexInSentence: Option[ReverseNodeIndex[TSentence]],
  charBegin: Int,
  charEnd: Int,
  tokenBegin: Int,
  var value: TimeValue
) {

  def getDocument = {
    if (indexInSentence.isEmpty)
      Option.empty
    else {
      val ts = indexInSentence.get.upperNode
      if (ts.indexInDocument.isEmpty) Option.empty
      else Some(ts.indexInDocument.get.upperNode)
    }
  }

  def getContextWithStart = {
    val sentenceIdx = indexInSentence.get
    (sentenceIdx.upperNode, sentenceIdx.position)
  }

  def toJson =
    ("text" -> tokens.map(_.text).mkString(" ")) ~
    ("charBegin" -> charBegin) ~
    ("charEnd" -> charEnd) ~
    ("tokenBegin" -> tokenBegin) ~
    ("value" -> value.toJson)

  override def toString: String = {
    compact(render(toJson))
  }

}
