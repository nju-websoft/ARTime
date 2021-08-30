package cn.edu.nju.websoft.artime.rule

import cn.edu.nju.websoft.artime.action._
import cn.edu.nju.websoft.artime.semantic.constant.Direction.Direction
import cn.edu.nju.websoft.artime.semantic.constant.TimeUnit.TimeUnit
import cn.edu.nju.websoft.artime.semantic.constant._
import cn.edu.nju.websoft.artime.struct.{TExpression, TexToken}
import cn.edu.nju.websoft.artime.utils.UInt
import com.codecommit.gll.{Failure, FailureData, RegexParsers, Success}
import org.json4s.native.JsonMethods.{parse, _}

import scala.{:+, ::}
import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer
import scala.io.Source


object RuleHandler {
  class actionsParser(
    variables: IndexedSeq[String]
  ) extends RegexParsers {
    def varSym: Parser[String] =
      "$" ~ "\\d+".r ^^ { (_, id) => variables(id.toInt) }

    def valueSym: Parser[UInt] =
      "Value:" ~ ("\\d+|X+".r | varSym) ^^ { (_, v) => if (v.startsWith("X")) UInt.unknownInt else UInt(v.toInt) }

    def directionSym: Parser[Direction] =
      "Direction:" ~ "(Past|Present|Future)".r ^^ { (_, v) => v.charAt(1) match {
        case 'a' => Direction.Past
        case 'r' => Direction.Present
        case 'u' => Direction.Future
      }}

    def grainSym: Parser[TimeUnit] =
      "TimeUnit:" ~ ("[A-Za-z]+".r | varSym) ^^ { (_, v) => TimeUnit.values.find(t => t.toString equals v).get }

    val enumTable: Map[String, (EnumConstants, EnumConstants#Value)] =
      Seq(Season, MonthOfYear, DayOfWeek, TimeOfDay).flatMap(e => {
        e.values.toSeq.map(v => v.toString -> (e, v))
      }).toMap
    def enumSym: Parser[(EnumConstants, EnumConstants#Value)] =
      "Enum:" ~ ("[A-Za-z]+".r | varSym) ^^ { (_, v) => enumTable(v) }

    def action: Parser[Action] =
      "Action:Ref" ~ directionSym ^^ { (_, d) => RefAction(d) } |
      "Action:Modify*" ~ enumSym ^^ { (_, e) => ModifyEnumAction(e._2, e._1) } |
      "Action:BasicModify" ~ valueSym ~ grainSym ~ opt(grainSym) ^^ { (_, v, g, up) => ModifyValAction(v, g, up) } |
      "Action:ToBegin" ~ grainSym ~ opt(grainSym) ^^ { (_, g, up) => ToBeginAction(g, up) } |
      "Action:ToEnd" ~ grainSym ~ opt(grainSym) ^^ { (_, g, up) => ToEndAction(g, up) } |
      "Action:Backward" ~ valueSym ~ grainSym ^^ { (_, v, g) => BackwardAction(v.value, g) } |
      "Action:Forward" ~ valueSym ~ grainSym ^^ { (_, v, g) => ForwardAction(v.value, g) } |
      "Action:ToLast" ~ grainSym ^^ { (_, g) => ToLastAction(g) } |
      "Action:ToNext" ~ grainSym ^^ { (_, g) => ToNextAction(g) } |
      "Action:Equal" ~ grainSym ^^ { (_, g) => EqualAction(g) } |
      "Action:MakeSet" ~ grainSym ^^ { (_, g) => MakeSetAction(g) } |
      "Action:AddTo" ~ valueSym ~ grainSym ^^ { (_, v, g) => AddToAction(v, g) } |
      "Action:Count" ~ valueSym ~ enumSym ~ grainSym ^^ { (_, v, e, g) => CountAction(v.value, e._2, e._1, g)}

    def ruleContent: Parser[IndexedSeq[Action]] =
      rep1(action) ^^ { as => as.toIndexedSeq }

    def parseRule(str: String): Either[FailureData, IndexedSeq[Action]] = {
      ruleContent(str).head match {
        case Success(matched, _) => Right(matched)
        case Failure(msg, _) => {
          println(str, msg)
          Left(msg)
        }
      }
    }
  }

  var record = mutable.Map.empty[String, String]
  def getType(t: String) = {
    if (!record.contains(t)) {
      val v = Rule.matchTable.find(kv => {
        kv._2.keys.exists(k => t.matches(k.takeWhile(_ != '/')))
      })
//      println(t, if (v.isDefined) v.get._1 else t)
      record.update(t, if (v.isDefined) v.get._1 else t)
    }
    record(t)
  }

  def samePatternOf(r1: Rule, r2: Rule): Boolean = {
    if (r1.texPattern.length == r2.texPattern.length) {
      r2.texPattern.zip(r1.texPattern).forall(xy => xy._1.equals(xy._2))
    }
    else false
  }

  def samePatternOf(t: TExpression, r: Rule, start: Int, end: Int): Boolean = {
    samePatternOf(t.tokens.slice(start, end), r)
  }

  def samePatternOf(r1: IndexedSeq[TexToken], r2: Rule): Boolean = {
    if (r1.length == r2.texPattern.length) {
      r2.texPattern.zip(r1).forall(xy => {
//        println(xy._1,xy._2)
        xy._1.matches(xy._2)
      })
    }
    else false
  }

  def getMeaning(t: TExpression, r: Rule, start: Int, end: Int): (Rule, IndexedSeq[Action]) = {
    getMeaning(t.tokens.slice(start, end), r)
  }

  def getMeaning(t: IndexedSeq[TexToken], r: Rule): (Rule, IndexedSeq[Action]) = {
    val rLs = r.texPattern
    val unsortedVars = rLs.indices.collect {
      case i if rLs(i).isVariable =>
        val id = rLs(i).asInstanceOf[VariableTokenPattern].varID
        val value = t(i).getQuantity
        id -> value
    }
    val variables = unsortedVars.sortBy(_._1).map(_._2)
    //      println(tex.tokens.map(_.stdStr).mkString(" "), rLs.mkString(" "), variables)
    (r, new actionsParser(variables).parseRule(r.valuePattern.mkString(" ")).right.get)
  }

  def findRuleComposition(t: TExpression) = {
    def isStopWord(id: Int): Boolean = {
      val token = t.tokens(id)
      token.lemma.matches("the|'|this|of|to|-") || token.tag.matches("IN|SYM")
    }
    val index = getRuleIndex
    val f = Array.ofDim[Seq[(Rule, IndexedSeq[Action])]](t.tokens.length + 1)
    f(0) = Seq.empty
    (1 to t.tokens.length).foreach(i => {
      val matched =
        (0 until i)
          .collect({
            case 0 =>
              (0, ruleSorted
                .find(r => samePatternOf(t, r, 0, i)))
             case j if f(j) != null =>
              (j, ruleSorted
                .find(r => {
                  (f(j).isEmpty || r.form == f(j).head._1.form) && samePatternOf(t, r, j, i)
                }))
          })
          .filter(_._2.nonEmpty)
          .map(jr => f(jr._1) :+ getMeaning(t, jr._2.get, jr._1, i))

      val matched2 =
        if (isStopWord(i - 1) && f(i - 1) != null)
          matched :+ f(i - 1)
        else
          matched
      if (matched2.nonEmpty)
        f(i) = matched2.minBy(r => {
          (r.size, if (r.isEmpty) -1 else r.map(xy => index(xy._1)).min)
        })
    })
    if (f(t.tokens.length) == null)
      Seq.empty
    else
      f(t.tokens.length)
  }

  def subPatternDistance(r1: TExpression, r2: Rule): Boolean = {
    if (r1.tokens.length >= r2.texPattern.length) {
      r1.tokens.map(t => r2.texPattern(0).matches(t))
      val f = Array.ofDim[Int](r2.texPattern.length)
      r1.tokens.indices.foreach(i => {
        (0 until i)
      })
      r2.texPattern.zip(r1.tokens).forall(xy => xy._1.matches(xy._2))
    }
    else false
  }


  lazy val filteredRules: List[Rule] = ruleSorted.foldLeft(List.empty[Rule])((result, now) => {
      if (result.exists(r => samePatternOf(now, r))) {
//        println(now)
        result
      }
      else
        now :: result
  }).reverse


  def getMeaning(tex: TExpression, rule:(String, (String, String))): (IndexedSeq[Action], String) = {
    val rLs = rule._1.split(" ")
    val variables = rLs.indices.collect {
      case i if rLs(i).matches(".+:\\$\\d+") =>
        val id = rLs(i).dropWhile(_ != '$').drop(1).toInt
        val v = tex.tokens(i).getQuantity
        (id, v)
    }.sortBy(_._1).map(_._2)
    println(tex, rule, variables)
    (new actionsParser(variables).parseRule(rule._2._1).right.get, rule._2._2)
  }

  val ruleSorted: IndexedSeq[Rule] = getRules(0)

  def getRules(percent: Int): IndexedSeq[Rule] ={
    val manual_rules=Source.fromFile("manual-rules.txt").getLines().map(l => Rule.extract(parse(l))).toArray
    val trained_rules=Source.fromFile("rules.txt").getLines().map(l => Rule.extract(parse(l))).toArray

    def generateNumbers(n: Int, range: Int): List[Int] = {
      val arr = ArrayBuffer.empty[Int]
      val r = scala.util.Random
      while(arr.size < n) {
        val value = r.nextInt(range)
        if(!arr.contains(value)) arr += value
      }
      arr.toList
    }
    val num=(manual_rules.size*(percent*0.01)).toInt
    //    println(num,manual_rules.size)
    val selected_id=generateNumbers(num,manual_rules.size)
    var selected_rules=Array[Rule]()
    var i=0
    for(i <- manual_rules.indices)
    {
      if(selected_id.contains(i)) {
        //          println(manual_rules(i))
        selected_rules :+= manual_rules(i)
      }
    }
    selected_rules++trained_rules
  }

  def getRuleIndex: Map[Rule, Int] = {
//    ruleSorted.foreach(println)
    ruleSorted.zipWithIndex.toMap
  }

  def main(args: Array[String]): Unit = {
    val parser = new actionsParser(IndexedSeq("1", "Decade"))
    println(parser.parseRule("Action:Backward Value:$0 TimeUnit:$1 Action:ToEnd TimeUnit:QuarterY TimeUnit:HalfY"))

//    println(getType("Friday"))
//    println(subPatternOf("Friday", "WEEK:$0"))
//    println(filteredRules.mkString("\n"))
  }

}
