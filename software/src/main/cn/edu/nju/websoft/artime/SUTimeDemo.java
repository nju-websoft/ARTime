package cn.edu.nju.websoft.artime;
import edu.stanford.nlp.ling.CoreAnnotations;
import edu.stanford.nlp.ling.CoreLabel;
import edu.stanford.nlp.pipeline.*;
import edu.stanford.nlp.time.*;
import cn.edu.nju.websoft.artime.io.TMLReader;
import edu.stanford.nlp.util.CoreMap;
import scala.Tuple3;

import java.io.File;
import java.util.*;

public class SUTimeDemo {

    public static String[] examples = {
            "In competitions against the clock, some athletes display an ability to seize control. Think of the Clark-Kent-to-Superman routines that John Elway and Michael Jordan often pulled in the final seconds.",
            "But Iram Leon stands on the sidelines of his own race against time. Lodged in his brain is an untreatable and inoperable cancerous tumor that statistics suggest will kill him before he is 40, eight years from now. Medical science is advancing at a rate that doesn't preclude the development of a treatment, but it's not clear if it will come in time.",
            "\"No one knows what technology will be available in five years,\" said Allan Friedman, Duke University Hospital neurosurgeon in chief, who in 2011 removed as much of Leon's brain tumor as possible.",
            "The torment of enduring that wait can paste a cancer patient to the couch, a surrender heavily associated with deadlier outcomes. Some seek escape in their careers, but that is no longer an option for Leon, who early this year was forced to step down as a juvenile probation officer in Travis County, Texas, a position he had held for almost seven years. His thinking is no longer clear, said Leon, adding, \"I was making too many mistakes on the stand.\"",
            "But Leon can still run. Two years after his brain-cancer diagnosis, he recently ran a sub-five-minute mile for the first time since high school. What has startled the medical community even more is what Leon did this month in Beaumont, Texas. He won the Gusher Marathon, finishing in 3:07:35. That was one second slower than his personal record in the 26.2-mile event, set days before he underwent brain surgery in early 2011."
    };

    public void demo(){
        // set up pipeline properties
        Properties props = new Properties();
        // general properties
        props.setProperty("annotators", "tokenize,ssplit,pos,lemma,ner");
        props.setProperty("ner.docdate.usePresent", "true");
        props.setProperty("sutime.includeRange", "true");
        props.setProperty("sutime.markTimeRanges", "true");
        // build pipeline
        StanfordCoreNLP pipeline = new StanfordCoreNLP(props);
        for (String example : examples) {
            CoreDocument document = new CoreDocument(example);
            pipeline.annotate(document);
            for (CoreEntityMention cem : document.entityMentions()) {
                System.out.println("temporal expression: "+cem.text());
                System.out.println("temporal value: " +
                        cem.coreMap().get(TimeAnnotations.TimexAnnotation.class));
            }
        }
    }

    public void demo2(){
        Properties props = new Properties();
        props.setProperty("sutime.restrictToTimex3","true");
        AnnotationPipeline pipeline = new AnnotationPipeline();
        pipeline.addAnnotator(new TokenizerAnnotator(false));
        pipeline.addAnnotator(new WordsToSentencesAnnotator(false));
        pipeline.addAnnotator(new POSTaggerAnnotator(false));
        pipeline.addAnnotator(new TimeAnnotator("sutime", props));
        for (String text : examples) {
            Annotation annotation = new Annotation(text);
            annotation.set(CoreAnnotations.DocDateAnnotation.class, "2013-07-14");
            pipeline.annotate(annotation);
            System.out.println(annotation.get(CoreAnnotations.TextAnnotation.class));
            List<CoreMap> timexAnnsAll = annotation.get(TimeAnnotations.TimexAnnotations.class);
            for (CoreMap cm : timexAnnsAll) {
                List<CoreLabel> tokens = cm.get(CoreAnnotations.TokensAnnotation.class);
                System.out.println(cm + " [from char offset " +
                        tokens.get(0).get(CoreAnnotations.CharacterOffsetBeginAnnotation.class) +
                        " to " + tokens.get(tokens.size() - 1).get(CoreAnnotations.CharacterOffsetEndAnnotation.class) + ']' +
                        " --> " + cm.get(TimeExpression.Annotation.class).getTemporal().getTimexValue() + cm.get(TimeExpression.Annotation.class).getTemporal().getTimexType());
            }
            System.out.println("--");
        }
    }

    public String recognizeAndNormalize(String filename)
    {
        // set up pipeline properties
        Tuple3 file=TMLReader.readFile4SUTime(new File(filename));
        String dctValue=file._1().toString();
        String text=file._2().toString();
        String text_copy="";
        String tml=file._3().toString();
//        System.out.println(tml);
//        System.out.println(dctValue);
//        System.out.println(text);
        Properties props = new Properties();
        props.setProperty("sutime.restrictToTimex3","true");
        AnnotationPipeline pipeline = new AnnotationPipeline();
        pipeline.addAnnotator(new TokenizerAnnotator(false));
        pipeline.addAnnotator(new WordsToSentencesAnnotator(false));
        pipeline.addAnnotator(new POSTaggerAnnotator(false));
        pipeline.addAnnotator(new TimeAnnotator("sutime", props));
        Annotation annotation = new Annotation(text);
        annotation.set(CoreAnnotations.DocDateAnnotation.class, dctValue);
        pipeline.annotate(annotation);
//        System.out.println(annotation.get(CoreAnnotations.TextAnnotation.class));
        List<CoreMap> timexAnnsAll = annotation.get(TimeAnnotations.TimexAnnotations.class);
        int tid=1,offset=0;
        List<String>labels=new ArrayList<>();
        for (CoreMap cm : timexAnnsAll) {
            List<CoreLabel> tokens = cm.get(CoreAnnotations.TokensAnnotation.class);
            String type=cm.get(TimeExpression.Annotation.class).getTemporal().getTimexType().toString();
            String value=cm.get(TimeExpression.Annotation.class).getTemporal().getTimexValue();
            int begin=tokens.get(0).get(CoreAnnotations.CharacterOffsetBeginAnnotation.class);
            int end=tokens.get(tokens.size() - 1).get(CoreAnnotations.CharacterOffsetEndAnnotation.class);
//            System.out.println(cm.toString());
            String timex=text.substring(begin,end);
//            System.out.println(text.substring(begin,end));
            text_copy+=text.substring(offset,begin)+"<T>";
            offset=end;
//            System.out.println(text_copy);
            labels.add("<TIMEX3 tid=\"t"+tid+"\" type=\""+type+"\" value=\""+value+"\">"+timex+"</TIMEX3>");
            tid+=1;
        }
        text_copy+=text.substring(offset);
        for(String s:labels){
            text_copy=text_copy.replaceFirst("<T>",s);
        }
//        System.out.println(text_copy);
        String tex = java.util.regex.Matcher.quoteReplacement(text_copy);
        return tml.replaceAll("<TEXT>[\\s\\S]+</TEXT>","<TEXT>"+tex+"</TEXT>");
    }


    public static void main(String[] args) {
        SUTimeDemo s=new SUTimeDemo();
        //        s.demo();
        System.out.println(s.recognizeAndNormalize("datasets/tempeval3/te3/nyt_20130321_china_pollution.tml"));
//        s.demo2();


    }
}
