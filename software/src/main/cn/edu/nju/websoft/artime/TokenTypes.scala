package cn.edu.nju.websoft.artime

import scala.io.Source

object TokenTypes {
  val name2regex: Map[String, String] = Source.fromFile("tokenTypes.tsv").getLines()
    .map(l => {
      val kv = l.split('\t')
      kv(0) -> kv(1)
    }).toMap
  val regex2name: Map[String, String] = name2regex.map(kv => kv._2 -> kv._1)
}
