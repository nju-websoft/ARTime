package cn.edu.nju.websoft.artime.struct

import cn.edu.nju.websoft.artime.utils.ReverseNodeIndex
import org.json4s
import org.json4s.JsonDSL._
import org.json4s.native.JsonMethods.{compact, render}


case class TSentence (
  tokens: IndexedSeq[TexToken],
  var indexInDocument: Option[ReverseNodeIndex[TDocument]],
  charBegin: Int,
  charEnd: Int,
  tokenBegin: Int,
  timexs: IndexedSeq[TExpression]
) {

  tokens.zipWithIndex.foreach(tkI => tkI._1.indexInSentence = Option(ReverseNodeIndex(this, tkI._2)))
  timexs.zipWithIndex.foreach(
    teI => teI._1.indexInSentence = Option(ReverseNodeIndex(this, teI._2))
  )

  def toJson: json4s.JObject =
    ("tokens" -> tokens.map(_.toString)) ~
    ("charBegin" -> charBegin) ~
    ("charEnd" -> charEnd) ~
    ("tokenBegin" -> tokenBegin)

  override def toString: String = {
    compact(render(toJson))
  }
}
