package cn.edu.nju.websoft.artime

import cn.edu.nju.websoft.artime.feature.FeatureGenerator.generateFeatures
import cn.edu.nju.websoft.artime.feature.FeaturedInterpretation
import cn.edu.nju.websoft.artime.io.TMLReader
import cn.edu.nju.websoft.artime.rule.{RuleAnalyzer, RuleHandler}
import cn.edu.nju.websoft.artime.semantic.{TimePointImpl, TimeValue}
import cn.edu.nju.websoft.artime.struct.TDataset
import de.bwaldvogel.liblinear._
import org.json4s.JsonAST
import org.json4s.JsonDSL._
import org.json4s.native.JsonMethods.{compact, render}

import java.io.{BufferedWriter, File, FileWriter}
import scala.io.Source
import scala.util.control.Breaks._

object Main {
  def makeRulesAndTrainingData(dataset: TDataset) = {
    val interpretations = generateFeatures(dataset, hasAnswer = true)
    val ranked = RuleAnalyzer.getStatistics(interpretations)

    val rWriter = new BufferedWriter(new FileWriter("rules.txt",false))
    ranked.foreach(r => {
      rWriter.write(r._1.toString)
      rWriter.newLine()
    })
    rWriter.close()
    val ruleSize= ranked.size
    println("ruleSize:"+ruleSize)
  }

  def writeTrainingData(input: Map[FeaturedInterpretation, Boolean]) = {
    input.keys.foreach(println)
    val rfWriter = new BufferedWriter(new FileWriter("rf.txt"))
    input.foreach(fy => {
      val y = if (fy._2) 1 else 0
      rfWriter.write(s"$y\t${fy._1.features.map(fn => s"${fn.getIndex}:${fn.getValue}").mkString("\t")}")
      rfWriter.newLine()
    })
    rfWriter.close()
    val dfWriter = new BufferedWriter(new FileWriter("df.txt"))
    input.withFilter(_._2).foreach(df => {
      val y = df._1.refType.id + 1
      dfWriter.write(s"$y\t${df._1.features.map(fn => s"${fn.getIndex}:${fn.getValue}").mkString("\t")}")
      dfWriter.newLine()
    })
    dfWriter.close()
  }

  def readPretrainedRules(dir: File):Unit={
    val rWriter = new BufferedWriter(new FileWriter("manual-rules.txt"))
    dir.listFiles().array.foreach(f=>{
      val file=Source.fromFile(f)
      for(lines<-file.getLines.drop(1))
        {
          breakable({
            val head=lines.split("\t")(0)
            if(head.equals("###")) break()
            val texPattern=lines.split("\t")(2).split(" ").toIndexedSeq
            val valuePattern=lines.split("\t")(3).split(" ").toIndexedSeq
            val form=lines.split("\t")(4)
            def toJson: JsonAST.JObject =
              ("texPattern" -> texPattern.map(_.toString)) ~ ("valuePattern" -> valuePattern) ~ ("form" -> form)
            val str=compact(render(toJson))
            rWriter.write(str)
            rWriter.newLine()
          })
        }
    })
    rWriter.close()
  }

  def mkdirSystem(testDatasetName: String)={
    val output_dir=new File("system")
    if(output_dir.exists())
    {
      output_dir.listFiles().foreach(f=>f.delete())
    }
    else {
      output_dir.mkdir()
    }
    val test_copy=new File(testDatasetName)
    test_copy.listFiles().foreach(f=>{
      val file=Source.fromFile(f)
      val rWriter = new BufferedWriter(new FileWriter("system/"+f.getName))
      for(line <- file.getLines())
      {
        rWriter.write(line)
        rWriter.newLine()
      }
      rWriter.close()
    })
  }

  def writeIntoSystem(f: FeaturedInterpretation,resultValue: TimeValue):Unit= {
//    println(f.tex.value.tid)
    var suc=0
    val file_name="system/"+f.texID.split(":")(1)
    //    println(file_name)
    var timex3type=resultValue.timex3type
    if(resultValue.isSet)
    {
      timex3type="SET"
    }
    val file=Source.fromFile(file_name)
    var filecontent: Array[String]= Array[String]()
    val p=("<TIMEX3[^<>]*tid=\"t"+f.tex.value.tid+"\"[^<>]*>").r
    for(line <- file.getLines()) {
      p.findFirstIn(line) match {
        case Some(s) => {
          //            println(s)
          val match_type= "type=\"[\\S]+\"".r
          val Type_filled=match_type.replaceFirstIn(s,"type=\""+timex3type+"\"")
          //          println(Type_filled)
          val match_value= "value=\"[\\S]+\"".r
          val Value_filled=match_value.replaceFirstIn(Type_filled,"value=\"" + resultValue.toTimex3Fmt + "\"")
          //          println(Value_filled)
          suc=1
          val l=line.replace(s,Value_filled)
          filecontent:+=l
        }
        case None =>{
          filecontent:+=line
        }
      }
    }
    if(suc==0) {
      println("output failed",file_name,f.tex.value.tid,resultValue.toTimex3Fmt)
    }
    file.close()
    val rWriter = new BufferedWriter(new FileWriter(file_name))
    for(line2 <- filecontent) {
      rWriter.write(line2)
      rWriter.newLine()
    }
    rWriter.close()
    //        val rWriter = new BufferedWriter(new FileWriter())
  }

  def removeFromSystem(file_name:String,tid: Int):Unit= {
    var suc=0
    val file=Source.fromFile(file_name)
    var filecontent: Array[String]= Array[String]()
    val p=("<TIMEX3[^<>]*tid=\"t"+tid+"\"([^<>]*)>([^<>]+)</TIMEX3>").r
    for(line <- file.getLines()) {
      p.findFirstIn(line) match {
        case Some(s) => {
          //          println(s)
          suc=1
          val p1=("<TIMEX3[^<>]*tid=\"t"+tid+"\"[^<>]*>").r
          val s1=p1.replaceFirstIn(s,"")
          val p2=("</TIMEX3>").r
          val s2=p2.replaceFirstIn(s1,"")
          val l=line.replace(s,s2)
          filecontent:+=l
        }
        case None =>{
          filecontent:+=line
        }
      }
    }
    if(suc==0) {
      println("remove failed",file_name,tid)
    }
    file.close()
    val rWriter = new BufferedWriter(new FileWriter(file_name))
    for(line2 <- filecontent) {
      rWriter.write(line2)
      rWriter.newLine()
    }
    rWriter.close()
  }

  def main(args: Array[String]): Unit = {
    //    readPretrainedRules(new File("pretrained-rules"))

    //train process on TempEval-3, Tweets or Tweets-modified
//    val trainDatasetName = "datasets/TempEval-3/T+A_corrected"
//    val trainDatasetName = "datasets/Tweets/trainingset"
    val trainDatasetName = "datasets/Tweets-modified/trainingset"
    val trainDataset = TMLReader.readDataset(new File(trainDatasetName),readValue = true)
    println("trainset:")
    makeRulesAndTrainingData(trainDataset)
    val trainTotal=trainDataset.timexs.size
    println("trainSize:"+trainTotal)
    println("-------------------------------------------------------------------------------")

    //test process
    //test gold mention
//    val testDatasetName="datasets/TempEval-3/te3-platinum"
//    val testDatasetName="datasets/Tweets/testset"
    val testDatasetName="datasets/Tweets-modified/testset"

    //test e2e performance
//        val testDatasetName="datasets/uwtime-te3"
    //    val testDatasetName="datasets/uwtime-tweets"
    //    val testDatasetName="datasets/uwtime-tweets-modified"
//    val testDatasetName="datasets/ptime-te3"
//    val testDatasetName="datasets/ptime-tweet"
//    val testDatasetName="datasets/tomn_te3"
//    val testDatasetName="datasets/tomn_tweets"
//    val testDatasetName="datasets/syntime_te3"
//    val testDatasetName="datasets/syntime_tweets"
    val testDataset = TMLReader.readDataset(new File(testDatasetName), readValue = true)

    println("total rules number:"+RuleHandler.ruleSorted.size)
    println("deduplicated rules number:"+RuleHandler.filteredRules.size)

    println("testset:")
    var index=0
    val testTotal=testDataset.timexs.size
    var positive=0
    var type_positive=0
    var recall=0.00
    var hasMeaning=0

    mkdirSystem(testDatasetName)

    var indexedList: List[Int]=List()
    generateFeatures(testDataset,false).foreach(f=>{
      index=f.texID.split(":")(0).toInt
      if(!indexedList.contains(index)) {
        hasMeaning+=1
        indexedList=indexedList:+index
        val ref = Some(f.tex.getDocument.get.documentTime.value.asInstanceOf[TimePointImpl])
        val resultValue= ExpressionHandler.executeMeaning(ref, f.rule.form, f.meaning)
        val result = resultValue.toTimex3Fmt
        var timex3type=resultValue.timex3type
        if(resultValue.isSet)
        {
          timex3type="SET"
        }
        //        println(f.tex.tokens,result,f.tex.value.toTimex3Fmt)

        writeIntoSystem(f,resultValue)

        if(result.equals(f.tex.value.toTimex3Fmt)) {
          positive+=1
//          println("true",result,f.tex.value.toTimex3Fmt,f.texID,f.tex.tokens.toString(),f.rule.texPattern)
        } else{
          println("false",result,f.tex.value.toTimex3Fmt,f.texID,f.tex.toString,f.rule.valuePattern)
        }

        if(timex3type.equals(f.tex.value.timex3type))
        {
          //            println("true",result,f.tex.value.toTimex3Fmt,f.texID,Type,f.tex.value.Type)
          type_positive+=1
        }
        else
        {
          //            println("Type_false",result,f.tex.value.toTimex3Fmt,f.texID,timex3type,f.tex.value.timex3type,f.rule.valuePattern)
        }
      }
    })

    println("can't be explained:")
    testDataset.timexs.indices.foreach(id=>{
      if(!indexedList.contains(id))
      {
        val file_name=testDataset.timexs(id).getDocument.get.name
        val tid=testDataset.timexs(id).value.tid
        println(file_name,tid,testDataset.timexs(id))
        removeFromSystem("system/"+file_name,tid)
      }
    })
    println("-------------------------------------------------------------------------------")

    println("can explain:"+hasMeaning+"\t"+"positive:"+positive+"\t"+"negative"+testTotal)
    recall=(positive*100.0)/testTotal
    val precision=(positive*100.0)/hasMeaning
    println("testSize:"+testTotal)
    println("-------------------------------------------------------------------------------")
    println("type positive:"+type_positive)
    println("Precision:"+precision+"%")
    println("recall:"+recall+"%")
    println("F1:"+(2*precision*recall)/(precision+recall)+"%")

  }

  //for SUTimeDemo
  def main2(args: Array[String]): Unit = {
    val s = new SUTimeDemo
//    val testDatasetName="datasets/tempeval3/te3"
    val testDatasetName="datasets/tweets_copy/testset"
    val testDataset=new File(testDatasetName)
    testDataset.listFiles().foreach(f=>{
      val filename=f.getName
      val rWriter = new BufferedWriter(new FileWriter("sutimeOutput/"+filename))
      rWriter.write(s.recognizeAndNormalize(testDatasetName+"/"+filename))
      rWriter.close()
    })
  }

  def testDF = {
    val problem = Problem.readFromFile(new File("df.txt"), 0.0)
    val model = Model.load(new File("df.model"))
    var (t1, f1, t2, f2, t3, f3) = (0, 0, 0, 0, 0, 0)
    problem.x.indices.foreach(i => {
      val scores = Array.ofDim[Double](3)

      val result = Linear.predictValues(model, problem.x(i), scores).round.toInt
      //scores.indices.withFilter(i => scores(i) > 0).map(_ + 1).toSet
      val gold = problem.y(i).round.toInt
      (result, (result == gold)) match {
        case (1, true) => t1 += 1
        case (1, false) => f1 += 1
        case (2, true) => t2 += 1
        case (2, false) => f2 += 1
        case (3, true) => t3 += 1
        case (3, false) => f3 += 1
      }
      println(i, gold, result, scores(0), scores(1), scores(2))

    })
    println(s"t1=$t1, f1=$f1, t2=$t2, f2=$f2, t3=$t3, f3=$f3")
  }

}
