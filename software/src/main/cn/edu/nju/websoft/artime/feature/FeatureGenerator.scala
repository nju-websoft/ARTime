package cn.edu.nju.websoft.artime.feature

import cn.edu.nju.websoft.artime.action.{Action, AddToAction, ModifyValAction, CountAction, MakeSetAction, ModifyAction, OffsetAction, RefAction}
import cn.edu.nju.websoft.artime.rule.RuleHandler.actionsParser
import cn.edu.nju.websoft.artime.rule.{Rule, RuleHandler, TokenPattern, VariableTokenPattern}
import cn.edu.nju.websoft.artime.semantic.constant.TimeUnit
import cn.edu.nju.websoft.artime.semantic.constant.TimeUnit.TimeUnit
import cn.edu.nju.websoft.artime.semantic.{TimeDuration, TimePointImpl, TimeRef}
import cn.edu.nju.websoft.artime.struct.{TDataset, TExpression}
import cn.edu.nju.websoft.artime.utils.{Counter, NumChecker}
import cn.edu.nju.websoft.artime.{ActionInfer, ExpressionHandler, SolutionSet}
import de.bwaldvogel.liblinear._
import shapeless.syntax.std.tuple._

import scala.collection.mutable
import scala.util.{Success, Try}

object FeatureGenerator {
  val ALLOW_COMPOSITION = true
  def getInferResult(tex: TExpression, ref: TExpression, numConstraint: Counter[Int]): SolutionSet = {
    val refValue = ref.value
    if (!refValue.isInstanceOf[TimePointImpl])
      return new SolutionSet()
    val realTokenLength = tex.tokens.count(t => !t.lemma.matches(",|:|-"))
    val defaultActions = tex.tokens.flatMap(_.parseAsDateTime).toMap.mapValues(x => List(x))
    val a = new ActionInfer(Some(ref.value.asInstanceOf[TimePointImpl]), numConstraint, realTokenLength, defaultActions)
//    println(tex.value, a.refPoint, numConstraint, tex.tokens, realTokenLength)
    val result = a.inference(tex.value)
//    println(result)
    //assert((0 until result.solutions.size).forall(i => !(0 until result.solutions.size).exists(j => i != j && result.solutions.toSeq(i) == result.solutions.toSeq(j))))
    result
  }

  def generateForSolution(tex: TExpression, solution: IndexedSeq[Action]) = {
    val form = tex.value match {
      case t: TimeRef => "REF"
      case t: TimePointImpl => "POINT"
      case t: TimeDuration => "DURATION"
    }
//    val tense = FeatureOfTense.makeValueForUpdate(tex)._2.map(x => s"tense:${x}")
//    val leadingIN = FeatureOfLeadingIN.makeValueForUpdate(tex)._2.map(x => s"leading:${x}")
//    val singular = FeatureOfSingularUnit.makeValueForUpdate(tex)._2.map(x => s"singularNoun:${x}")
//    val features = (tense /*++ leadingIN*/ ++ singular).toSet
    val r = Rule.makeRule(tex.tokens, solution, form)
      (r, IndexedSeq.empty[StructuredFeature]/*(
        FeatureOfPattern.makeValue(r.texPattern),
        FeatureOfConstant.makeValue(r.texPattern),
        FeatureOfRuleVar.makeValue(r.texPattern),
        FeatureOfParameters.makeValue(solution),
      )*/, solution)
  }

  def generateForCandidateMeaning(r: Rule, solution: IndexedSeq[Action]) = {
    (r, IndexedSeq.empty[StructuredFeature]/*(
      FeatureOfPattern.makeValue(r.texPattern),
      FeatureOfConstant.makeValue(r.texPattern),
      FeatureOfRuleVar.makeValue(r.texPattern),
      FeatureOfParameters.makeValue(solution),
    )*/, solution)
  }

  def generateForSolutionSet(tex: TExpression, ref: TExpression, numConstraint: Counter[Int]) = {
    val solutions: SolutionSet = getInferResult(tex, ref, numConstraint)
//    println(solutions.solutions.size)
    val allIntepretations = solutions.solutions.map(s => generateForSolution(tex, s.toIndexedSeq))
    allIntepretations.filter(x =>
      !allIntepretations.exists(p =>
        x._1.texPattern == p._1.texPattern && hasProperSubSolution(x._3, p._3))
    )
  }

  def hasProperSubSolution(target: Seq[Action], refer: Seq[Action]): Boolean = {
    def looseUp(a: Action): Action = {
      a match {
        case a: ModifyAction => ModifyValAction(a.v, a.g, Option.empty)
        case a => a
      }
    }
    val s = target.map(looseUp).toSet
    refer.map(looseUp).toSet != s && refer.map(looseUp).forall(s.contains)
  }

  def mergeComposition(meanings: Seq[(Rule, IndexedSeq[Action])]) = {
    def getGrain(action: Action): Option[TimeUnit] = action match {
        case a: AddToAction  => Some(a.g)
        case a: OffsetAction => Some(a.g)
        case a: ModifyAction => Some(a.g)
        case a: CountAction => Some(a.g.grain)
        case a: MakeSetAction => Some(a.g)
        case _ => Option.empty
      }

    if (meanings.nonEmpty) {
      val (tPat, vPat) = meanings.map(_._1)
        .foldLeft((mutable.IndexedSeq.empty[TokenPattern], mutable.IndexedSeq.empty[String]))((B, r) => {
          (B._1 ++ r.texPattern, B._2 ++ r.valuePattern)
        })
      val sortedActions = meanings.flatMap(_._2).sortWith {
        case (_: RefAction, _) =>
          false
        case (a1, a2) => {
          (getGrain(a1), getGrain(a2)) match {
            case (g1, g2) if g1.isEmpty || g2.isEmpty =>
              false
            case (g1, g2) if g1 != g2 =>
              g1.get < g2.get
            case (g1, g2) if g1 == g2 =>
              a1.priority < a2.priority
          }
        }
      }.toIndexedSeq
      IndexedSeq((Rule(tPat, vPat, meanings.head._1.form), sortedActions))
    } else
      IndexedSeq.empty
  }

  def getCandidateMeanings(tex: TExpression) = {
//    println(RuleHandler.ruleSorted.size)
    val singleRuleMeanings = RuleHandler.ruleSorted.filter(r =>
      RuleHandler.samePatternOf(tex.tokens, r)
    ).map(r => RuleHandler.getMeaning(tex.tokens, r))
    if (singleRuleMeanings.isEmpty && ALLOW_COMPOSITION)
      mergeComposition(RuleHandler.findRuleComposition(tex))
    else {
      singleRuleMeanings
    }
  }

  def getFeatureNodes(value: IndexedSeq[Double]) = {
    value.zipWithIndex.withFilter(_._1 > 0.0).map(vk => new FeatureNode(vk._2 + 1, vk._1).asInstanceOf[Feature]).toArray
  }

  def cross(a1: IndexedSeq[Double], a2: IndexedSeq[Double]) = {
    a1.flatMap(x => a2.map(y => x * y))
  }

  def generateFeatures(dataset: TDataset, hasAnswer: Boolean) = {
    val timexs = dataset.timexs
//    val instances = new Instances("", texAttributes, 0)

    def generateFeatures(id: Int): IndexedSeq[FeaturedInterpretation] = {
      val tex = timexs(id)
      val idStr = s"$id:${tex.getDocument.get.name}:${tex.tokenBegin}"
      val numbers = tex.tokens.map(_.lemma).map(NumChecker.collectNumber).collect { case Success(x) => x }
      val numConstraint = new Counter[Int](numbers)
      val tf = IndexedSeq.empty[StructuredFeature]/*(
        FeatureOfTexQuantity.makeValue(tex),
        FeatureOfLeadingIN.makeValue(tex),
        FeatureOfTense.makeValue(tex)
      )*/
      val solutions = mutable.ArrayBuilder.make[(Rule, IndexedSeq[StructuredFeature], IndexedSeq[Action], RefType.Value)]()
      if (hasAnswer) {
//        if (id > 0 && tex.getDocument == timexs(id - 1).getDocument) {
//          val prev = timexs(id - 1)
//          generateForSolutionSet(tex, prev, numConstraint).foreach(s => solutions += (s :+ RefType.PREV))
//        }
//        if (id + 1 < timexs.length && tex.getDocument == timexs(id + 1).getDocument) {
//          val succ = timexs(id + 1)
//          generateForSolutionSet(tex, succ, numConstraint).foreach(s => solutions += (s :+ RefType.SUCC))
//        }
        val dct = tex.getDocument.get.documentTime
        generateForSolutionSet(tex, dct, numConstraint).foreach(s => solutions += (s :+ RefType.DCT))

        solutions.result().map(sf => {
          val sparsed = getFeatureNodes(cross(tf.flatMap(_.toDoubles), sf._2.flatMap(_.toDoubles)))
          FeaturedInterpretation(idStr, tex, sf._1, sf._4, sf._3, tex.value, sparsed)
        })
      }
      else {
        //TODO: reference time refer needed
        val ref = Some(tex.getDocument.get.documentTime.value.asInstanceOf[TimePointImpl])
        val meanings = getCandidateMeanings(tex)
        meanings.map(m => {
          Try{
            val newValue = ExpressionHandler.executeMeaning(ref, m._1.form, m._2)
            val rf = generateForCandidateMeaning(m._1, m._2)
            val sparsed = getFeatureNodes(cross(tf.flatMap(_.toDoubles),  rf._2.flatMap(_.toDoubles)))
            FeaturedInterpretation(idStr, tex, m._1, RefType.DCT, m._2, newValue, sparsed)
          }
        }).collect {
          case Success(x) => x
        }
      }
    }

    val texData = timexs.indices.flatMap(generateFeatures)
    texData
  }
}

