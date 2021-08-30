package cn.edu.nju.websoft.artime.struct

import cn.edu.nju.websoft.artime.utils.ReverseNodeIndex
import org.json4s.native.JsonMethods.{pretty, render}
import org.json4s.JsonDSL._

case class TDocument (
  name: String,
  sentences: IndexedSeq[TSentence],
  var indexInDataset: Option[ReverseNodeIndex[TDataset]],
  charSize: Int,
  timexs: IndexedSeq[TExpression],
  documentTime: TExpression,
  rawText: String
) {
  sentences.zipWithIndex.foreach(seI => seI._1.indexInDocument = Option(ReverseNodeIndex(this, seI._2)))
  override def toString: String = {
    val json =
      ("size" -> charSize) ~
      ("sentences" -> sentences.map(_.toJson)) ~
      ("DCT" -> documentTime.toJson) ~
      ("timexs" -> timexs.map(_.toJson))
    pretty(render(json))
  }
}
