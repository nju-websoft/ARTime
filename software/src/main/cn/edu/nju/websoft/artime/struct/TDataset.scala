package cn.edu.nju.websoft.artime.struct

import cn.edu.nju.websoft.artime.utils.ReverseNodeIndex
import org.json4s.native.JsonMethods.{pretty, render}
import org.json4s.JsonDSL._


case class TDataset (
  documents: IndexedSeq[TDocument]
) {
  val timexs: IndexedSeq[TExpression] = documents.flatMap(_.timexs)
  documents.zipWithIndex.foreach(docI => docI._1.indexInDataset = Option(ReverseNodeIndex(this, docI._2)))

  override def toString: String = pretty(render(documents.map(_.toString)))
}
