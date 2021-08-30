package cn.edu.nju.websoft.artime.utils

import cn.edu.nju.websoft.artime.struct.TexToken

import scala.util.Try

object NumChecker {
  def getNumberType(token: TexToken) = {
    token.tag match {
      case "JJ"|"RB"|"ORDINAL"|"NNP"|"NN"|"NNPS"|"NNS" => "ORD"
      case "CD"|"LS" => "NUM"
      case "YEAR" => "YEAR"
      case "DECADE" => "DECADE"
      case "compDECADE" => "compDECADE"
      case _ => "???"
    }
  }

  val timeRE1 = "([0-2]?[0-9]):([0-5]?[0-9]):([0-5]?[0-9])".r
  val timeRE2 = "([0-2]?[0-9]):([0-5]?[0-9])".r
  def analyzeTime(token: TexToken) = {
    token.text match {
      case timeRE1(hour, minute, second) =>
        s"[H:${hour}][M:${minute}][S:${second}]"
      case timeRE2(hour, minute) =>
        s"[H:${hour}][M:${minute}]"
    }
  }

  val dateRE1 = "([1-9]|[0-3][0-9])[-/.]([1-9]|[0-3][0-9])[-/.]([1-2][0-9]{3})".r
  val dateRE2 = "([12][0-9]{3})[-/.](0?[1-9]|1[0-2])[-/.]([0-3][0-9])".r
  val dateRE3 = "([1-9]{2})[-/.](0?[1-9]|1[0-2])[-/.]([0-3][0-9])".r
  def analyzeDate(token: TexToken) = token.text match {
    case dateRE1(month, day, year) =>
      if (month.toInt > 12)
        s"[Y:${year}][M:${day}][D:${month}]"
      else
        s"[Y:${year}][M:${month}][D:${day}]"
    case dateRE2(year, month, day) =>
      s"[Y:${year}][M:${month}][D:${day}]"
    case dateRE3(year, month, day) =>
      s"[Y:${year}][M:${month}][D:${day}]"
  }

  def analyzeDecade(token: TexToken) = token.text match {
    case "twenties" =>
      "2"
    case "thirties" =>
      "3"
    case "forties" =>
      "4"
    case "fifties" =>
      "5"
    case "sixties" =>
      "6"
    case "seventies" =>
      "7"
    case "eighties" =>
      "8"
    case "nineties" =>
      "9"
    case s: String if (s.matches("([1-2]?[0-9])?[0-9]0s")) =>
      token.text.dropRight(2)
  }

  val headR = "twenty|thirty|forty|fifty|sixty|seventy|eighty|ninety"
  val tailR1 = "one|two|three|four|five|six|seven|eight|nine"
  val tailR2 = "first|second|third|fourth|fifth|sixth|seventh|eighth|ninth"
  val splitRE = s"($headR)-($tailR1|$tailR2)".r

  def collectNumber(x: String): Try[Int] = x.toLowerCase() match {
    case "1st"|"first" => Try(1)
    case "2nd"|"second" => Try(2)
    case "3rd"|"third" => Try(3)
    case "4th"|"fourth" => Try(4)
    case "5th"|"fifth" => Try(5)
    case "6th"|"sixth" => Try(6)
    case "7th"|"seventh" => Try(7)
    case "8th"|"eighth" => Try(8)
    case "9th"|"ninth" => Try(9)
    case "10th"|"tenth" => Try(10)
    case "11th"|"eleventh" => Try(11)
    case "12th"|"twelfth" => Try(12)
    case "13th"|"thirteenth" => Try(13)
    case "14th"|"fourteenth" => Try(14)
    case "15th"|"fifteenth" => Try(15)
    case "16th"|"sixteenth" => Try(16)
    case "17th"|"seventeenth" => Try(17)
    case "18th"|"eighteenth" => Try(18)
    case "19th"|"nineteenth" => Try(19)
    case "20th"|"twentieth" => Try(20)
    case "21st"|"twenty\\-first" => Try(21)
    case "22nd"|"twenty\\-second" => Try(22)
    case "23rd"|"twenty\\-third" => Try(23)
    case "24th"|"twenty\\-fourth" => Try(24)
    case "25th"|"twenty\\-fifth" => Try(25)
    case "26th"|"twenty\\-sixth" => Try(26)
    case "27th"|"twenty\\-seventh" => Try(27)
    case "28th"|"twenty\\-eighth" => Try(28)
    case "29th"|"twenty\\-ninth" => Try(29)
    case "30th"|"thirtieth" => Try(30)
    case "31st"|"thirtieth\\-first" => Try(31)
    case "a" => Try(1)
    case "one" => Try(1)
    case "two" => Try(2)
    case "three" => Try(3)
    case "four" => Try(4)
    case "five" => Try(5)
    case "six" => Try(6)
    case "seven" => Try(7)
    case "eight" => Try(8)
    case "nine" => Try(9)
    case "ten" => Try(10)
    case "eleven" => Try(11)
    case "twelve" => Try(12)
    case "thirteen" => Try(13)
    case "fourteen" => Try(14)
    case "fifteen" => Try(15)
    case "sixteen" => Try(16)
    case "seventeen" => Try(17)
    case "eighteen" => Try(18)
    case "nineteen" => Try(19)
    case "twenty" => Try(20)
    case "thirty" => Try(30)
    case "forty" => Try(40)
    case "fifty" => Try(50)
    case "sixty" => Try(60)
    case "seventy" => Try(70)
    case "eighty" => Try(80)
    case "ninety" => Try(90)
    case "hundred" => Try(100)
    case "thousand" => Try(1000)
    case splitRE(head, tail) => Try(collectNumber(head).get + collectNumber(tail).get)
    case x => Try(x.toInt)
  }
}
