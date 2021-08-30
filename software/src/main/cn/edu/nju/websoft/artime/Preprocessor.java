package cn.edu.nju.websoft.artime;

import cn.edu.nju.websoft.artime.rule.Rule;
import cn.edu.nju.websoft.artime.struct.TexToken;
import cn.edu.nju.websoft.artime.utils.NumChecker;
import edu.stanford.nlp.simple.Sentence;
import scala.Option;
import scala.Tuple2;
import scala.collection.immutable.Map;
import scala.util.Try;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashMap;
import java.util.List;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import static java.util.regex.Pattern.CASE_INSENSITIVE;


public class Preprocessor {
	static HashMap<String, Integer> numValues;

	static {
		numValues = new HashMap<>();
		numValues.put("a", 1);          numValues.put("one", 1);	    	numValues.put("two", 2);		numValues.put("three", 3);
		numValues.put("four", 4);		numValues.put("five", 5);		    numValues.put("six", 6);	    numValues.put("seven", 7);
		numValues.put("eight", 8);		numValues.put("nine", 9);		    numValues.put("ten", 10);  	    numValues.put("eleven", 11);
		numValues.put("twelve", 12);	numValues.put("thirteen", 13);		numValues.put("fourteen", 14);	numValues.put("fifteen", 15);
		numValues.put("sixteen", 16);	numValues.put("seventeen", 17);	    numValues.put("eighteen", 18);	numValues.put("nineteen", 19);
		numValues.put("twenty", 20);	numValues.put("thirty", 30);		numValues.put("forty", 40);	    numValues.put("fifty", 50);
		numValues.put("sixty", 60);	    numValues.put("seventy", 70);		numValues.put("eighty", 80);	numValues.put("ninety", 90);
		numValues.put("hundred", 100);  numValues.put("thousand", 1000);
	}

	public static List<TexToken> preprocess(List<TexToken> rawTokens) {
		ArrayList<TexToken> tokenBuffer = new ArrayList<>(rawTokens);
		List<String> num1 = Arrays.asList("a", "one", "two", "three", "four", "five", "six", "seven", "eight", "nine", "ten", "eleven", "twelve", "thirteen", "fourteen", "fifteen", "sixteen", "seventeen", "eighteen", "nineteen", "twenty");
		List<String> num2 = Arrays.asList("twenty", "thirty", "forty", "fifty", "sixty", "seventy", "eighty", "ninety");
/*
		//holiday
		Pattern p = Pattern.compile("christmas|xmas|easter|halloween|thanksgiving", CASE_INSENSITIVE);
		for (TaggedToken t : tokenBuffer){
			if (p.matcher(t.text()).matches()){
				t.posTag_$eq("HOLIDAY");
			}
		}
*/
/*
		Pattern p1 = Pattern.compile("new", CASE_INSENSITIVE);
		Pattern p2 = Pattern.compile("year", CASE_INSENSITIVE);
		for (int i = 0; i < tokenBuffer.size() - 1; i++){
			TaggedToken t1 = tokenBuffer.get(i);
			TaggedToken t2 = tokenBuffer.get(i + 1);
			if (p1.matcher(t1.text()).matches() && p2.matcher(t2.text()).matches()){
				tokenBuffer.remove(i + 1);
				tokenBuffer.set(i, new TaggedToken(t1.text()+"___"+t2.text(), t1.lemma()+"___"+t2.lemma(), "HOLIDAY", t1.charBegin(), t2.charEnd(), null));
			}
		}
*/

/*
		p1 = Pattern.compile("election|inauguration|groundhog|valentine|patrick|memorial|columbus", CASE_INSENSITIVE);
		p2 = Pattern.compile("year|day", CASE_INSENSITIVE);
		for (int i = 0; i < tokenBuffer.size() - 1; i++){
			TaggedToken t1 = tokenBuffer.get(i);
			TaggedToken t2 = tokenBuffer.get(i + 1);
			if (p1.matcher(t1.text()).matches() && p2.matcher(t2.text()).matches()){
				tokenBuffer.remove(i + 1);
				tokenBuffer.set(i, new TaggedToken(t1.text()+"___"+t2.text(), t1.lemma()+"___"+t2.lemma(), "HOLIDAY", t1.charBegin(), t2.charEnd(), null));
			}
		}
		p1 = Pattern.compile("father|mother|valentine|patrick|fools", CASE_INSENSITIVE);
		p2 = Pattern.compile("day", CASE_INSENSITIVE);
		for (int i = 0; i < tokenBuffer.size() - 2; i++){
			TaggedToken t1 = tokenBuffer.get(i);
			TaggedToken t2 = tokenBuffer.get(i + 1);
			TaggedToken t3 = tokenBuffer.get(i + 2);
			if (p1.matcher(t1.text()).matches() && p2.matcher(t3.text()).matches() && t2.posTag().matches("POS")){
				tokenBuffer.remove(i + 2);
				tokenBuffer.remove(i + 1);
				tokenBuffer.set(i, new TaggedToken(t1.text()+"___"+t2.text()+"___"+t3.text(), t1.lemma()+"___"+t2.lemma()+"___"+t3.lemma(), "HOLIDAY", t1.charBegin(), t3.charEnd(), null));
			}
		}
*/
		for (TexToken t : tokenBuffer){
			if (t.tag().equals("LS")){
				t.tag_$eq("CD");
			}
		}

		for (int i = 0; i < tokenBuffer.size() - 1; i++) {
			TexToken t1 = tokenBuffer.get(i);
			TexToken t2 = tokenBuffer.get(i + 1);
			if (t1.lemma().matches("ten|eleven|twelve|thirteen|fourteen|fifteen|sixteen|seventeen|eighteen|nineteen")
					&& t2.text().matches("twenties|thirties|forties|fifties|sixties|seventies|eighties|nineties")) {
				tokenBuffer.remove(i + 1);
				tokenBuffer.set(i, new TexToken(t1.text() + "___" + t2.text(), String.valueOf(num1.indexOf(t1.lemma())*10 + num2.indexOf(t2.lemma())+2), "compDECADE", t1.charBegin(), t2.charEnd(), null));
			}
			if (t1.lemma().matches("twenty")
					&& t2.text().matches("twenties|thirties|forties|fifties|sixties|seventies|eighties|nineties")) {
				tokenBuffer.remove(i + 1);
				tokenBuffer.set(i, new TexToken(t1.text() + "___" + t2.text(), String.valueOf(200 + num2.indexOf(t2.lemma())+2), "compDECADE", t1.charBegin(), t2.charEnd(), null));
			}

		}

		for(int i=0;i<tokenBuffer.size();i++)
		{
			TexToken t1 = tokenBuffer.get(i);
			if (t1.text().matches("([1-2]?[0-9])[0-9]0s")) {
				tokenBuffer.set(i, new TexToken(t1.text(), NumChecker.analyzeDecade(t1), "compDECADE", t1.charBegin(), t1.charEnd(), null));
			}
		}

		//DECADE
		for (TexToken t1 : tokenBuffer) {
			if (t1.text().matches("twenties|thirties|forties|fifties|sixties|seventies|eighties|nineties|[0-9]0s")) {
				t1.tag_$eq("DECADE");
				t1.qType_$eq(Option.apply("DECADE"));
				t1.lemma_$eq(NumChecker.analyzeDecade(t1));
			}
		}

		for (int i = 0; i < tokenBuffer.size(); i++) {
			TexToken t1 = tokenBuffer.get(i);
			if (t1.text().matches("[0-2]?[0-9]:[0-5]?[0-9]:[0-5]?[0-9]")) {
				String[] TIME= t1.text().split("[:]");
				if(TIME.length!=3)
				{
					continue;
				}
				int end0=t1.charBegin()+TIME[0].length();
				tokenBuffer.set(i,new TexToken(TIME[0], TIME[0], "CD", t1.charBegin(), end0, null));
				int end1=end0+1;
				tokenBuffer.add(i+1,new TexToken(":",":","SYM",end0, end1, null));
				int end2=end1+TIME[1].length();
				tokenBuffer.add(i+2,new TexToken(TIME[1],TIME[1],"CD",end1, end2, null));
				int end3=end2+1;
				tokenBuffer.add(i+3,new TexToken(":",":","HYPH",end2, end3, null));
				int end4=end3+TIME[2].length();
				tokenBuffer.add(i+4,new TexToken(TIME[2],TIME[2],"CD",end3, end4, null));
			}

			if (t1.text().matches("[0-2]?[0-9]:[0-5]?[0-9]")) {
				String[] TIME= t1.text().split("[:]");
				if(TIME.length!=2)
				{
					continue;
				}
				int end0=t1.charBegin()+TIME[0].length();
				tokenBuffer.set(i,new TexToken(TIME[0], TIME[0], "CD", t1.charBegin(), end0, null));
				int end1=end0+1;
				tokenBuffer.add(i+1,new TexToken(":",":","SYM",end0, end1, null));
				int end2=end1+TIME[1].length();
				tokenBuffer.add(i+2,new TexToken(TIME[1],TIME[1],"CD",end1, end2, null));
			}

			if (t1.text().matches("[0-2]?[0-9][.][0-5]?[0-9][.][0-5]?[0-9]")) {
				String[] TIME= t1.text().split("[.]");
				if(TIME.length!=3)
				{
					continue;
				}
				int end0=t1.charBegin()+TIME[0].length();
				tokenBuffer.set(i,new TexToken(TIME[0], TIME[0], "CD", t1.charBegin(), end0, null));
				int end1=end0+1;
				tokenBuffer.add(i+1,new TexToken(".",".","SYM",end0, end1, null));
				int end2=end1+TIME[1].length();
				tokenBuffer.add(i+2,new TexToken(TIME[1],TIME[1],"CD",end1, end2, null));
				int end3=end2+1;
				tokenBuffer.add(i+3,new TexToken(".",".","HYPH",end2, end3, null));
				int end4=end3+TIME[2].length();
				tokenBuffer.add(i+4,new TexToken(TIME[2],TIME[2],"CD",end3, end4, null));
			}

			if (t1.text().matches("[0-2]?[0-9][.][0-5]?[0-9]")) {
				String[] TIME= t1.text().split("[.]");
				if(TIME.length!=2)
				{
					continue;
				}
				int end0=t1.charBegin()+TIME[0].length();
				tokenBuffer.set(i,new TexToken(TIME[0], TIME[0], "CD", t1.charBegin(), end0, null));
				int end1=end0+1;
				tokenBuffer.add(i+1,new TexToken(".",".","SYM",end0, end1, null));
				int end2=end1+TIME[1].length();
				tokenBuffer.add(i+2,new TexToken(TIME[1],TIME[1],"CD",end1, end2, null));
			}

			else if (t1.text().matches("[0-3]?[0-9][.][0-3]?[0-9][.][1-2][0-9]{3}|" +
					"[12][0-9]{3}[.][0-3]?[0-9][.][0-3]?[0-9]|" +
					"[1-9]{2}[.][0-3]?[0-9][.][0-3]?[0-9]|" +
					"[0-3]?[0-9][.][0-3]?[0-9][.][1-9]{2}"))
			{
				String[] DATE= t1.text().split("[.]");
//				System.out.println(DATE.length);
				if(DATE.length!=3)
				{
					continue;
				}
				int end0=t1.charBegin()+DATE[0].length();
				tokenBuffer.set(i,new TexToken(DATE[0], DATE[0], "CD", t1.charBegin(), end0, null));
				int end1=end0+1;
				tokenBuffer.add(i+1,new TexToken(".",".","SYM",end0, end1, null));
				int end2=end1+DATE[1].length();
				tokenBuffer.add(i+2,new TexToken(DATE[1],DATE[1],"CD",end1, end2, null));
				int end3=end2+1;
				tokenBuffer.add(i+3,new TexToken(".",".","HYPH",end2, end3, null));
				int end4=end3+DATE[2].length();
				tokenBuffer.add(i+4,new TexToken(DATE[2],DATE[2],"CD",end3, end4, null));
			}
			else if (t1.text().matches("[0-3]?[0-9][/][0-3]?[0-9][/][1-2][0-9]{3}|" +
					"[12][0-9]{3}[/][0-3]?[0-9][/][0-3]?[0-9]|" +
					"[1-9]{2}[/][0-3]?[0-9][/][0-3]?[0-9]|" +
					"[0-3]?[0-9][/][0-3]?[0-9][/][1-9]{2}"))
			{
				String[] DATE= t1.text().split("[/]");
//				System.out.println(DATE.length);
				if(DATE.length!=3)
				{
					continue;
				}
				int end0=t1.charBegin()+DATE[0].length();
				tokenBuffer.set(i,new TexToken(DATE[0], DATE[0], "CD", t1.charBegin(), end0, null));
				int end1=end0+1;
				tokenBuffer.add(i+1,new TexToken("/","/","SYM",end0, end1, null));
				int end2=end1+DATE[1].length();
				tokenBuffer.add(i+2,new TexToken(DATE[1],DATE[1],"CD",end1, end2, null));
				int end3=end2+1;
				tokenBuffer.add(i+3,new TexToken("/","/","HYPH",end2, end3, null));
				int end4=end3+DATE[2].length();
				tokenBuffer.add(i+4,new TexToken(DATE[2],DATE[2],"CD",end3, end4, null));
			}
		}

		//split Number + Character
		for (int i = 0; i < tokenBuffer.size(); i++) {
			TexToken t = tokenBuffer.get(i);
			if (TokenTypes.name2regex().values().exists(t.stdStr()::matches)) continue;
			Matcher m = Pattern.compile("^[0-9]+[a-z]", CASE_INSENSITIVE).matcher(t.text());
			if (m.find()) {
				int k = m.end()-1;
				tokenBuffer.set(i, new TexToken(t.text().substring(0, k), t.text().substring(0, k), "CD", t.charBegin(), t.charBegin()+k, null));
				tokenBuffer.add(i+1, new TexToken(t.text().substring(k), new Sentence(t.text().substring(k)).lemma(0), t.tag(), t.charBegin()+k, t.charEnd(), null));
			}
		}

		for (int i = 0; i < tokenBuffer.size() - 2; i++){
			TexToken t1 = tokenBuffer.get(i);
			TexToken t2 = tokenBuffer.get(i + 1);
			TexToken t3 = tokenBuffer.get(i + 2);
			if (t1.text().matches("[aA]") && t2.text().matches("few") && t3.text().matches("of")){
				tokenBuffer.remove(i + 2);
				tokenBuffer.remove(i + 1);
				tokenBuffer.set(i, new TexToken(t1.text()+"___"+t2.text()+"___"+t3.text(), "a_few_of", "IQ", t1.charBegin(), t3.charEnd(), null));
			}
			else if (t1.text().matches("[aA]") && t2.text().matches("couple") && t3.text().matches("of")){
				tokenBuffer.remove(i + 2);
				tokenBuffer.remove(i + 1);
				tokenBuffer.set(i, new TexToken(t1.text()+"___"+t2.text()+"___"+t3.text(), "a_couple_of", "IQ", t1.charBegin(), t3.charEnd(), null));
			}
			else if (t1.text().matches("[aA]") && t2.text().matches("dozen") && t3.text().matches("of")){
				tokenBuffer.remove(i + 2);
				tokenBuffer.remove(i + 1);
				tokenBuffer.set(i, new TexToken(t1.text()+"___"+t2.text()+"___"+t3.text(), "a_dozen_of", "IQ", t1.charBegin(), t3.charEnd(), null));
			}
		}
		for (int i = 0; i < tokenBuffer.size() - 1; i++){
			TexToken t1 = tokenBuffer.get(i);
			TexToken t2 = tokenBuffer.get(i + 1);
			if (t1.text().matches("[aA]") && t2.text().matches("few")){
				tokenBuffer.remove(i + 1);
				tokenBuffer.set(i, new TexToken(t1.text()+"___"+t2.text(), "a_few", "IQ", t1.charBegin(), t2.charEnd(), null));
			}
			else if (t1.text().matches("[aA]") && t2.text().matches("couple")){
				tokenBuffer.remove(i + 1);
				tokenBuffer.set(i, new TexToken(t1.text()+"___"+t2.text(), "a_couple", "IQ", t1.charBegin(), t2.charEnd(), null));
			}
			else if (t1.text().matches("[aA]") && t2.text().matches("dozen")){
				tokenBuffer.remove(i + 1);
				tokenBuffer.set(i, new TexToken(t1.text()+"___"+t2.text(), "a_dozen", "IQ", t1.charBegin(), t2.charEnd(), null));
			}
			else if (t1.text().matches("[sS]everal") || t1.text().matches("[fF]ew") || t1.text().matches("[cC]ouple") || t1.text().matches("[dD]ozen")){
				t1.tag_$eq("IQ");
			}
		}

		for (int i = 0; i < tokenBuffer.size() - 2; i++){
			TexToken t1 = tokenBuffer.get(i);
			TexToken t2 = tokenBuffer.get(i + 1);
			TexToken t3 = tokenBuffer.get(i + 2);
			if (t1.lemma().matches("(?i)twenty|thirty|forty|fifty|sixty|seventy|eighty|ninety")
					&& t2.lemma().matches("-")
					&& t3.lemma().matches("one|two|three|four|five|six|seven|eight|nine")) {
				tokenBuffer.remove(i + 2);
				tokenBuffer.remove(i + 1);
				tokenBuffer.set(i, new TexToken(t1.text()+"___"+t3.text(), String.valueOf((num2.indexOf(t1.lemma())+2)*10+num1.indexOf(t3.lemma())), "CD", t1.charBegin(), t3.charEnd(), null));
			}

			else if (t1.lemma().matches("(?i)twenty|thirty|forty|fifty|sixty|seventy|eighty|ninety")
					&& t2.lemma().matches("-")
					&& t3.lemma().matches("first|second|third|fourth|fifth|sixth|seventh|eighth|ninth")) {
				tokenBuffer.remove(i + 2);
				tokenBuffer.remove(i + 1);
				tokenBuffer.set(i, new TexToken(t1.text()+"___"+t3.text(), t1.lemma()+t2.lemma()+t3.lemma(), "ORDINAL", t1.charBegin(), t3.charEnd(), null));
			}

			else if(t1.lemma().matches("(?i)one|two|three|four|five|six|seven|eight|nine")
					&& t2.lemma().matches("-")
					&& t3.lemma().matches("thousand|thousands")
			)
			{
				tokenBuffer.remove(i + 2);
				tokenBuffer.remove(i + 1);
				tokenBuffer.set(i, new TexToken(t1.text()+"___"+t3.text(), String.valueOf(num1.indexOf(t1.lemma())*1000), "CD", t1.charBegin(), t3.charEnd(), null));
			}
		}

		for (int i = 0; i < tokenBuffer.size() - 1; i++){
			TexToken t1 = tokenBuffer.get(i);
			TexToken t2 = tokenBuffer.get(i + 1);
			if (t1.lemma().matches("[tT]wenty|[tT]hirty|[fF]orty|[fF]ifty|[sS]ixty|[sS]eventy|[eE]ighty|[nN]inety")
					&& t2.lemma().matches("one|two|three|four|five|six|seven|eight|nine")){
				tokenBuffer.remove(i + 1);
				tokenBuffer.set(i, new TexToken(t1.text()+"___"+t2.text(), String.valueOf((num2.indexOf(t1.lemma())+2)*10+num1.indexOf(t2.lemma())), "CD", t1.charBegin(), t2.charEnd(), null));
			}
		}

		for (int i = 0; i < tokenBuffer.size(); i++){
			TexToken t1 = tokenBuffer.get(i);
			if (t1.lemma().matches("(twenty|thirty|forty|fifty|sixty|seventy|eighty|ninety)-(one|two|three|four|five|six|seven|eight|nine)")){
				tokenBuffer.set(i, new TexToken(t1.text(), String.valueOf((num2.indexOf(t1.lemma().split("-")[0])+2)*10 + num1.indexOf(t1.lemma().split("-")[1])), "CD", t1.charBegin(), t1.charEnd(), null));
			}
			else if (t1.lemma().matches("one|two|three|four|five|six|seven|eight|nine|ten|eleven|twelve|thirteen|fourteen|fifteen|sixteen|seventeen|eighteen|nineteen")){
				tokenBuffer.set(i, new TexToken(t1.text(), String.valueOf((num1.indexOf(t1.lemma()))), "CD", t1.charBegin(), t1.charEnd(), null));
			}
			else if (t1.lemma().matches("a")){
				tokenBuffer.set(i, new TexToken(t1.text(), "1", "CD", t1.charBegin(), t1.charEnd(), null));
			}
			else if (t1.lemma().matches("twenty|thirty|forty|fifty|sixty|seventy|eighty|ninety") && t1.tag().matches("CD")){
				tokenBuffer.set(i, new TexToken(t1.text(), String.valueOf((num2.indexOf(t1.lemma().split("-")[0])+2)*10), "CD", t1.charBegin(), t1.charEnd(), null));
			}
		}

		for (int i = 0; i < tokenBuffer.size(); i++){
			TexToken t1 = tokenBuffer.get(i);
			if (t1.text().matches("[Ff]ollowing"))
			{
				tokenBuffer.set(i, new TexToken(t1.text(), "following", "NEXT", t1.charBegin(), t1.charEnd(), null));
			}
			else if (t1.text().matches("[Cc]oming"))
			{
				tokenBuffer.set(i, new TexToken(t1.text(), "coming", "VBG", t1.charBegin(), t1.charEnd(), null));
			}
		}

		for (int i = 0; i < tokenBuffer.size() - 1; i++){
			TexToken t1 = tokenBuffer.get(i);
			TexToken t2 = tokenBuffer.get(i + 1);
			if (t1.lemma().matches("[12][0-9]")&& !t1.text().matches("[12][0-9]")
					&& t2.lemma().matches("[1-9][0-9]")){
				tokenBuffer.remove(i + 1);
				tokenBuffer.set(i, new TexToken(t1.text()+"___"+t2.text(), t1.lemma()+t2.lemma(), "YEAR", t1.charBegin(), t2.charEnd(), null));
			}
			else if (t1.lemma().matches("[1-9]") && t2.lemma().matches("thousand")){
				tokenBuffer.remove(i + 1);
				int n1 = Integer.parseInt(t1.lemma());
				int n2 = 1000;
				tokenBuffer.set(i, new TexToken(t1.text()+"___"+t2.text(), String.valueOf(n1*n2), "CD", t1.charBegin(), t2.charEnd(), null));
			}
			else if (t1.lemma().matches("[1-9]|1[0-9]") && t2.lemma().matches("hundred")){
				tokenBuffer.remove(i + 1);
				int n1 = Integer.parseInt(t1.lemma());
				int n2 = 100;
				tokenBuffer.set(i, new TexToken(t1.text()+"___"+t2.text(), String.valueOf(n1*n2), "CD", t1.charBegin(), t2.charEnd(), null));
			}
		}

		for (int i = 0; i < tokenBuffer.size() - 2; i++) {
			TexToken t1 = tokenBuffer.get(i);
			TexToken t2 = tokenBuffer.get(i + 1);
			TexToken t3 = tokenBuffer.get(i + 2);
			if (((t1.text().endsWith("thousand") || t1.text().endsWith("thousands")) && t1.lemma().matches("[1-9]000")) && ((t2.text().endsWith("hundred") || t2.text().endsWith("hundreds")) && t2.lemma().matches("[1-9]00"))){
				tokenBuffer.remove(i + 1);
				tokenBuffer.set(i, new TexToken(t1.text()+"___"+t2.text(), t1.lemma().substring(0, 1)+t2.lemma(), "CD", t1.charBegin(), t2.charEnd(), null));
			}

			t1 = tokenBuffer.get(i);
			t2 = tokenBuffer.get(i + 1);
			t3 = tokenBuffer.get(i + 2);
			if (t1.lemma().matches("[0-9]?[0-9]00") && t2.text().matches("and") && t3.lemma().matches("[0-9]?[0-9]")){
				tokenBuffer.remove(i + 2);
				tokenBuffer.remove(i + 1);
				int n1 = Integer.parseInt(t1.lemma());
				int n2 = Integer.parseInt(t3.lemma());
				tokenBuffer.set(i, new TexToken(t1.text()+"___"+t2.text()+"___"+t3.text(), String.valueOf(n1+n2), "CD", t1.charBegin(), t3.charEnd(), null));
			}
		}


		//CD+half
		for (int i = 0; i < tokenBuffer.size() - 3; i++){
			TexToken t1 = tokenBuffer.get(i);
			TexToken t2 = tokenBuffer.get(i + 1);
			TexToken t3 = tokenBuffer.get(i + 2);
			TexToken t4 = tokenBuffer.get(i + 3);
			if (t1.tag().matches("CD") && t2.text().matches("and") && t3.text().matches("a") && t4.text().matches("half")){
				tokenBuffer.remove(i + 3);
				tokenBuffer.remove(i + 2);
				tokenBuffer.remove(i + 1);
				tokenBuffer.set(i, new TexToken(t1.text()+"___"+t2.text()+"___"+t3.text()+"___"+t4.text(), t1.lemma()+".5", "CD", t1.charBegin(), t4.charEnd(), null));
			}
			else if (t1.tag().matches("CD") && t2.text().matches("and") && t3.text().matches("half")){
				tokenBuffer.remove(i + 2);
				tokenBuffer.remove(i + 1);
				tokenBuffer.set(i, new TexToken(t1.text()+"___"+t2.text()+"___"+t3.text(), t1.lemma()+".5", "CD", t1.charBegin(), t3.charEnd(), null));
			}
		}

		//PART_WORD
		for (int i = 0; i < tokenBuffer.size() - 2; i++) {
			TexToken t1 = tokenBuffer.get(i);
			TexToken t2 = tokenBuffer.get(i + 1);
			TexToken t3 = tokenBuffer.get(i + 2);
			if (t1.lemma().matches("the") && t2.lemma().matches("middle") && t3.lemma().matches("of")) {
				tokenBuffer.remove(i + 2);
				tokenBuffer.remove(i + 1);
				tokenBuffer.set(i, new TexToken(t1.text() + "_" + t2.text() + "_" + t3.text(), "the_middle_of", "PART_WORD", t1.charBegin(), t3.charEnd(), null));
			}
			else if (t1.lemma().matches("the") && t2.lemma().matches("end") && t3.lemma().matches("of")) {
				tokenBuffer.remove(i + 2);
				tokenBuffer.remove(i + 1);
				tokenBuffer.set(i, new TexToken(t1.text() + "_" + t2.text() + "_" + t3.text(), "the_end_of", "PART_WORD", t1.charBegin(), t3.charEnd(), null));
			}
			else if (t1.lemma().matches("the") && t2.lemma().matches("beginning") && t3.lemma().matches("of")) {
				tokenBuffer.remove(i + 2);
				tokenBuffer.remove(i + 1);
				tokenBuffer.set(i, new TexToken(t1.text() + "_" + t2.text() + "_" + t3.text(), "the_beginning_of", "PART_WORD", t1.charBegin(), t3.charEnd(), null));
			}
			else if (t1.lemma().matches("the") && t2.lemma().matches("start") && t3.lemma().matches("of")) {
				tokenBuffer.remove(i + 2);
				tokenBuffer.remove(i + 1);
				tokenBuffer.set(i, new TexToken(t1.text() + "_" + t2.text() + "_" + t3.text(), "the_start_of", "PART_WORD", t1.charBegin(), t3.charEnd(), null));
			}
		}

		//right now
		for (int i = 0; i < tokenBuffer.size() - 1; i++) {
			TexToken t1 = tokenBuffer.get(i);
			TexToken t2 = tokenBuffer.get(i + 1);
			if (t1.lemma().matches("right") && t2.lemma().matches("now")) {
				tokenBuffer.remove(i + 1);
				tokenBuffer.set(i, new TexToken(t1.text() + "_" + t2.text(), "right_now", "NOW", t1.charBegin(), t2.charEnd(), null));
			}
		}

		//INEQ+CD
		for (int i = 0; i < tokenBuffer.size() - 2; i++){
			TexToken t1 = tokenBuffer.get(i);
			TexToken t2 = tokenBuffer.get(i + 1);
			TexToken t3 = tokenBuffer.get(i + 2);
			if (t1.text().matches("[nN]o") && t2.text().matches("more") && t3.text().matches("than")){
				tokenBuffer.remove(i + 2);
				tokenBuffer.remove(i + 1);
				tokenBuffer.set(i, new TexToken(t1.text()+"_"+t2.text()+"_"+t3.text(), "no_more_than", "INEQ", t1.charBegin(), t3.charEnd(), null));
			}
			else if (t1.text().matches("[nN]o") && t2.text().matches("less") && t3.text().matches("than")){
				tokenBuffer.remove(i + 2);
				tokenBuffer.remove(i + 1);
				tokenBuffer.set(i, new TexToken(t1.text()+"_"+t2.text()+"_"+t3.text(), "no_less_than", "INEQ", t1.charBegin(), t3.charEnd(), null));
			}
			else if (t1.text().matches("[nN]o") && t2.text().matches("longer") && t3.text().matches("than")){
				tokenBuffer.remove(i + 2);
				tokenBuffer.remove(i + 1);
				tokenBuffer.set(i, new TexToken(t1.text()+"_"+t2.text()+"_"+t3.text(), "no_longer_than", "INEQ", t1.charBegin(), t3.charEnd(), null));
			}
		}
		for (int i = 0; i < tokenBuffer.size() - 2; i++){
			TexToken t1 = tokenBuffer.get(i);
			TexToken t2 = tokenBuffer.get(i + 1);
			if (t1.text().matches("[aA]t") && t2.text().matches("least")){
				tokenBuffer.remove(i + 1);
				tokenBuffer.set(i, new TexToken(t1.text()+"_"+t2.text(), "at_least", "INEQ", t1.charBegin(), t2.charEnd(), null));
			}
			else if (t1.text().matches("[aA]t") && t2.text().matches("most")){
				tokenBuffer.remove(i + 1);
				tokenBuffer.set(i, new TexToken(t1.text()+"_"+t2.text(), "at_most", "INEQ", t1.charBegin(), t2.charEnd(), null));
			}
			else if (t1.text().matches("[mM]ore") && t2.text().matches("than")){
				tokenBuffer.remove(i + 1);
				tokenBuffer.set(i, new TexToken(t1.text()+"_"+t2.text(), "more_than", "INEQ", t1.charBegin(), t2.charEnd(), null));
			}
			else if (t1.text().matches("[lL]onger") && t2.text().matches("than")){
				tokenBuffer.remove(i + 1);
				tokenBuffer.set(i, new TexToken(t1.text()+"_"+t2.text(), "longer_than", "INEQ", t1.charBegin(), t2.charEnd(), null));
			}
			else if (t1.text().matches("[lL]ess") && t2.text().matches("than")){
				tokenBuffer.remove(i + 1);
				tokenBuffer.set(i, new TexToken(t1.text()+"_"+t2.text(), "less_than", "INEQ", t1.charBegin(), t2.charEnd(), null));
			}
			else if (t1.text().matches("[uU]p") && t2.text().matches("to")){
				tokenBuffer.remove(i + 1);
				tokenBuffer.set(i, new TexToken(t1.text()+"_"+t2.text(), "up_to", "INEQ", t1.charBegin(), t2.charEnd(), null));
			}
			else if (t1.text().matches("[cC]lose") && t2.text().matches("to")){
				tokenBuffer.remove(i + 1);
				tokenBuffer.set(i, new TexToken(t1.text()+"_"+t2.text(), "close_to", "INEQ", t1.charBegin(), t2.charEnd(), null));
			}
			else if (t1.text().matches("[oO]ver") && t1.tag().matches("IN")){
				tokenBuffer.set(i, new TexToken(t1.text(), "over", "INEQ", t1.charBegin(), t1.charEnd(), null));
			}
			else if (t1.text().matches("[aA]") && t2.text().matches("mere")){
				tokenBuffer.remove(i + 1);
				tokenBuffer.set(i, new TexToken(t1.text()+"_"+t2.text(), "a_mere", "INEQ", t1.charBegin(), t2.charEnd(), null));
			}
		}

    	//YEAR
		for (TexToken t1 : tokenBuffer) {
			if (t1.lemma().matches("[1-2][0-9]{3}|'[0-9]{2}")) {
				t1.tag_$eq("YEAR");
				t1.qType_$eq(Option.apply("YEAR"));
			}
		}

		if(tokenBuffer.size()>=2)
		{
			int i=tokenBuffer.size() - 2;
			TexToken t1 = tokenBuffer.get(i);
			TexToken t2 = tokenBuffer.get(i + 1);
			if(t1.text().matches("(:?)(a|one)") && t2.text().matches("(:?)second"))
			{
				tokenBuffer.set(i+1, new TexToken(t2.text(), "seconds", "JJ", t2.charBegin(), t2.charEnd(), null));
			}
		}

		for (TexToken t1 : tokenBuffer) {
			Try<Object> oTry = NumChecker.collectNumber(t1.lemma());
			if (oTry.isSuccess()) {
				t1.lemma_$eq(oTry.get().toString());
				t1.qType_$eq(Option.apply(NumChecker.getNumberType(t1)));
			}
			else {
				Option<Tuple2<String, Map<String, String>>> pattern =
						Rule.matchTable().find(x -> x._2.keys().exists(k -> t1.lemma().matches(k)));
				if (pattern.nonEmpty()) {
					if (TexToken.pTagSet().contains(pattern.get()._1)) {
						t1.pType_$eq(Option.apply(pattern.get()._1));
					} else
						t1.qType_$eq(Option.apply(pattern.get()._1));
				}
			}
		}

		return tokenBuffer;
	}

}
