package cn.edu.nju.websoft.artime.rule

import cn.edu.nju.websoft.artime.action.Action
import cn.edu.nju.websoft.artime.struct.TexToken
import cn.edu.nju.websoft.artime.utils.NumChecker
import org.json4s.{DefaultFormats, JString, JsonAST}
import org.json4s.JsonAST.{JArray, JValue}
import org.json4s.JsonDSL._
import org.json4s.native.JsonMethods.{parse, _}

import scala.+:
import scala.collection.mutable
import scala.collection.mutable.ListBuffer
import scala.io.Source

case class Rule(
  texPattern: IndexedSeq[TokenPattern],
  valuePattern: IndexedSeq[String],
  form: String
//  feature: Set[String]
) {
  private def stringSym = compact(render(toJson))
  override def equals(obj: Any): Boolean = {
    obj.isInstanceOf[Rule] && obj.asInstanceOf[Rule].stringSym == stringSym
  }

  override def hashCode(): Int = stringSym.hashCode()
  def toJson: JsonAST.JObject =
    ("texPattern" -> texPattern.map(_.toString)) ~ ("valuePattern" -> valuePattern) ~ ("form" -> form)// ~ ("feature" -> feature)
  override def toString: String = stringSym

//  def splitByFeature: Seq[Rule] = {
//    Rule(this.texPattern, this.valuePattern, this.form, Set.empty) +:
//      (this.feature
//      .map(f => Rule(this.texPattern, this.valuePattern, this.form, Set(f)))
//      .toSeq)
//  }

}

object Rule {
  private val varRE = "(.+):\\$(\\d+)".r
  private val qRE = "(.+):([^$]+)".r
  private def parsePattern(s: String): TokenPattern = {
    s match {
      case varRE(name, id) => VariableTokenPattern(name, id.toInt)
      case qRE(name, _) => QuantityTypePattern(name)
      case x if(TexToken.pTagSet.contains(x))=>
        {
          PlainTexTypePattern(x)
        }
      case x => RawTokenPattern(x)
    }
  }

  def extract(json: JValue): Rule = {
    implicit val formats = DefaultFormats
    val texPattern = (json \ "texPattern").extract[IndexedSeq[String]].map(parsePattern)
//    val features = (json \ "feature").extract[Set[String]]
    Rule(texPattern, (json \ "valuePattern").extract[IndexedSeq[String]], (json \ "form").extract[String]/*, features*/)
  }

  private def mapping(tokens: IndexedSeq[TexToken], params: IndexedSeq[String]) = {
    var mappingCnt = 0
    tokens.indices.flatMap(i => params.indices.map(j => (i, j))).collect {
      case (i, j) if isMatchable(tokens(i), params(j)) => (i, j)
    }
  }

  private def makeDistinct(mapping: IndexedSeq[(Int, Int)]) = {
    val usedRecord = mutable.Set.empty[Int]
    mapping.groupBy(_._2).toIndexedSeq.sortBy(_._1).map(x => {
      val splited = x._2.map(_._1).groupBy(usedRecord.contains)
      val use = if (!splited.contains(false) || splited(false).isEmpty) {
        splited(true).min
      } else
        splited(false).min
      usedRecord += use
      use -> x._1
    })
  }

  def makeRule(tokens: IndexedSeq[TexToken], meaning: IndexedSeq[Action], resultType: String): Rule = {
    val params: IndexedSeq[String] = meaning.flatMap(_.getParameterStrings)
    val maps = makeDistinct(mapping(tokens, params))
    val tokenMaps = maps.map(_._1).distinct.zipWithIndex.toMap
    val paramMaps = maps.map(mi => mi._2 -> mi._1).toMap
    val mappedTokens = tokens.indices.map {
      case i if tokenMaps.contains(i) => VariableTokenPattern(tokens(i).qType.get, tokenMaps(i))
        //泛化一些常见的非数值的语言模式
      case i if tokens(i).pType.nonEmpty => PlainTexTypePattern(tokens(i).pType.get)
//      case i if tokens(i).qType.nonEmpty => QuantityTypePattern(tokens(i).qType.get)
      case i => {
        RawTokenPattern(tokens(i).lemma)
      }
    }
    val mappedParams = params.indices.map {
      case i if paramMaps.contains(i) => params(i).replaceFirst(":.+", s":\\$$${tokenMaps(paramMaps(i))}")
      case i => params(i)
    }

    Rule(mappedTokens, mappedParams, resultType)
  }


  implicit val formats = DefaultFormats
  val matchTable: Map[String, Map[String, String]] =
    parse(Source.fromFile("nominal.json").mkString).asInstanceOf[JArray].arr
    .map(tv => {
      val key = (tv \ "type").extract[String]
      val values = (tv \ "values").asInstanceOf[JArray].arr.map(rv => {
        (rv \ "regex").extract[String] -> (rv \ "value").extract[String]
      }).toMap
      key -> values
    }).toMap


  private def isMatchable(token: TexToken, param: String) = {
    if (param.startsWith("Value:")) {
      val g = NumChecker.collectNumber(param.drop(6))
      val p = NumChecker.collectNumber(token.lemma)
      if (g.isSuccess && p.isSuccess)
        g.get == p.get
      else
        false
    } else if (matchTable.contains(token.qType.getOrElse("NOT Q"))) {
      val table = matchTable(token.qType.getOrElse("NOT Q"))
      val key = table.keys.find(k => token.lemma.matches(k)).getOrElse("Nothing")
      table.getOrElse(key, "Nothing") == param
    } else
      false
  }
}
