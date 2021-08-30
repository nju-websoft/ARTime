package cn.edu.nju.websoft.artime.io

import java.io.File

import cn.edu.nju.websoft.artime.Preprocessor
import cn.edu.nju.websoft.artime.semantic.{TimePointImpl, TimeRef, TimeValue}
import cn.edu.nju.websoft.artime.struct.{TDataset, TDocument, TExpression, TSentence, TexToken}
import edu.stanford.nlp.simple.{Document, Sentence, Token}
import edu.stanford.nlp.util.StringUtils
import javax.lang.model.element.ModuleElement.Directive

import scala.collection.JavaConverters._
import scala.collection.mutable
import scala.io.Source
import scala.xml.{Elem, XML}

object TMLReader {
  def readDataset(dir: File, readValue: Boolean): TDataset = {
    assert(dir.isDirectory)
    TDataset(dir.listFiles().array
      .withFilter(_.getName.endsWith(".tml"))
      .map(f => readFile(f, readValue)))
  }
  def readFile4SUTime(file: File) ={
    val xml = XML.loadFile(file)
//    println(file)
    //parsing dct
    val dctAnn = (xml \ "DCT" \ "TIMEX3").head.asInstanceOf[Elem]
    val dctValue = TIMEX3Parser.parseAnnotation(dctAnn).right.get.toTimex3Fmt

    val rawText = (xml \ "TEXT").head.text.replaceAll("<TIMEX3[^<>]*>","").replaceAll("</TIMEX3>","")
    val result = (dctValue, rawText, Source.fromFile(file).getLines().mkString("\n"))
    result
  }
  def readFile(file: File, readValue: Boolean): TDocument = {
//    println(file)
    val xml = XML.loadFile(file)

    //parsing dct
    val dctAnn = (xml \ "DCT" \ "TIMEX3").head.asInstanceOf[Elem]
    val dctValue = TIMEX3Parser.parseAnnotation(dctAnn).right.get
    val dctContent = (xml \ "DCT" \ "TIMEX3").head.text
    val dctTokens = new Sentence(dctContent).tokens().asScala.toIndexedSeq.map(TexToken(_))
    val dct = TExpression(dctTokens, Option.empty, 0,  dctContent.length, 0, dctValue)

    //parsing text
    val rawText = (xml \ "TEXT").head.toString()
      .replaceAll("((?<!\\d+|[tT]wenty|[tT]hirty|[fF]orty|[fF]ifty|[sS]ixty|[sS]eventy|[eE]ighty|[nN]inety)-)|(-(?!one|two|three|four|five|six|seven|eight|nine|first|second|third|fourth|fifth|sixth|seventh|eighth|ninth))", " - ")
      .replaceAll("(?<![<\\d])/", " / ")
/*
      .replaceAll("([0-2]?[0-9]):([0-5]?[0-9]):([0-5]?[0-9])", "$1 : $2 : $3")
      .replaceAll("([0-2]?[0-9]):([0-5]?[0-9])", "$1 : $2")
			.replaceAll("([1-9]|[0-3][0-9])[-/.]([1-9]|[0-3][0-9])[-/.]([1-2][0-9]{3})", "$3 - $1 - $2")
      .replaceAll("([12][0-9]{3})[-/.](0?[1-9]|1[0-2])[-/.]([0-3][0-9])", "$1 - $2 -$3")
      .replaceAll("([1-9]{2})[-/.](0?[1-9]|1[0-2])[-/.]([" +
        "0-3][0-9])", "$1 - $2 - $3")
*/
    val doc = new Document(StringUtils.argsToProperties("resources/TMLparse.properties"), rawText)
    val documentBuilder = mutable.ArrayBuilder.make[TSentence]
    var charOffset = 0
    var tokenCnt = 0
    doc.sentences().asScala.foreach(rawSent => {
      val teRecords = mutable.ArrayBuilder.make[TExpression]
      val texBuilder = mutable.ArrayBuilder.make[TexToken]
      var inTex = false
      val sentenceBuilder = mutable.ArrayBuilder.make[TexToken]
      var lastValue: TimeValue = dctValue
      val sentenceTokenBegin = tokenCnt
      var texBegin = tokenCnt

      rawSent.tokens().asScala.foreach(rawToken => {
        rawToken.originalText() match {
          case s: String if s.matches("<TIMEX3.+>") =>
            val parseResult = TIMEX3Parser.parseAnnotation(s)
            if (parseResult.isRight) {
              if (readValue)
                lastValue = parseResult.right.get
              inTex = true
              texBegin = tokenCnt
            } else println(s, parseResult)
          case "</TIMEX3>" =>
            if (inTex) {
              inTex = false
              val tokens = texBuilder.result()
              val cBegin = tokens.head.charBegin
              val cEnd = tokens.last.charEnd
              val processed = Preprocessor.preprocess(tokens.toList.asJava).asScala.toArray
              teRecords += TExpression(processed, Option.empty, cBegin, cEnd, texBegin - sentenceTokenBegin, lastValue)
              sentenceBuilder ++= processed
              texBuilder.clear()
            }
          case s: String if s.matches("<.+>") =>
          case _ =>
            val length = rawToken.characterOffsetEnd() - rawToken.characterOffsetBegin()
            val token = TexToken(rawToken)
            charOffset += length + 1
            if (inTex) texBuilder += token
            else sentenceBuilder += token
            tokenCnt += 1
        }
      })
      val tokens = sentenceBuilder.result()
      if (tokens.nonEmpty) {
        val cBegin = tokens.head.charBegin
        val cEnd = tokens.last.charEnd
//        val processed = Preprocessor.preprocess(tokens.toList.asJava).asScala.toArray
        val texs: Array[TExpression] = recoverOmittedTokens(teRecords.result(), tokens)
        documentBuilder += TSentence(tokens, Option.empty, cBegin, cEnd, sentenceTokenBegin, texs)
      }
    })
    val sentences = documentBuilder.result()
    val size = sentences.last.charEnd
    //"<?xml version=\"1.0\" ?>" + xml
    TDocument(file.getName, sentences, Option.empty, size, sentences.flatMap(_.timexs), dct, Source.fromFile(file).mkString)
  }

  def recoverOmittedTokens(oldTEXs: Array[TExpression], sentence: Array[TexToken]): Array[TExpression] = {
    def prevCheck(tex: TExpression, pos: Int) = {
      tex.tokenBegin + tex.tokens.length == pos - 1 &&
        sentence(pos - 1).lemma.matches("-|and|to") &&
        tex.tokens.last.qType.getOrElse("").matches("YEAR|NUM")
    }
    def succCheck(tex: TExpression, pos: Int) = {
      tex.tokenBegin == pos + 2 &&
        sentence(pos + 1).lemma.matches("-|and|to") &&
        tex.tokens.head.qType.contains("NUM")
    }
    oldTEXs.indices.map(i => {
      val ex = oldTEXs(i)
      val pos = ex.tokenBegin
      if (ex.tokens.length == 1 && ex.tokens.head.qType.contains("NUM")) {
        if (i > 0 && prevCheck(oldTEXs(i - 1), pos)) {
          //for one token cases like 2020 - 21
          if (oldTEXs(i - 1).tokens.length == 1 && oldTEXs(i - 1).tokens.head.qType.contains("YEAR")) {
            val oldToken = ex.tokens.head
            val oldVal = oldToken.text
            val refToken = oldTEXs(i - 1).tokens.head
            val newVal = refToken.text.dropRight(oldVal.length) + oldVal
            val newToken = TexToken(
              oldVal, newVal, refToken.tag, oldToken.charBegin, oldToken.charEnd, oldToken.indexInSentence
            )
            newToken.qType = Some("YEAR")
            TExpression(
              IndexedSeq(newToken),
              ex.indexInSentence,
              ex.charBegin,
              ex.charEnd,
              ex.tokenBegin,
              ex.value
            )
          } else {
            TExpression(
              oldTEXs(i - 1).tokens.dropRight(1) ++ ex.tokens,
              ex.indexInSentence,
              ex.charBegin,
              ex.charEnd,
              ex.tokenBegin,
              ex.value
            )
          }
        } else if (i + 1 < oldTEXs.length && succCheck(oldTEXs(i + 1), pos)) {
          TExpression(
            ex.tokens ++ oldTEXs(i + 1).tokens.drop(1),
            ex.indexInSentence,
            ex.charBegin,
            ex.charEnd,
            ex.tokenBegin,
            ex.value
          )
        } else
          ex
      } else
        ex
    })
  }.toArray
}


