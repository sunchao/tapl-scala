package arith

import common._

import scala.util.parsing.combinator.syntactical.StandardTokenParsers
import java.io.FileReader
import scala.io.Source

class ArithParser extends StandardTokenParsers {
  lexical.delimiters ++= List("(",")",";")
  lexical.reserved ++= List("true","false","succ","pred","iszero","if","then","else")
  
  def term: Parser[Term] = (
      appTerm
    | "if" ~ term ~ "then" ~ term ~ "else" ~ term ^^
      { case _~pred~_~left~_~right => TmIf(pred,left,right) }
  )
  
  def aTerm: Parser[Term] = (
      "(" ~> term <~ ")"
    | "true" ^^ (x => TmTrue)
    | "false" ^^ (x => TmFalse)
    | numericLit ^^ { x =>
        def f(n: Int): Term = n match {
          case 0 => TmZero
          case n if n > 0 => TmSucc(f(n-1))
        }
        f(x.toInt)
      }
  )
  
  def appTerm: Parser[Term] = (
      aTerm
    | "succ" ~ aTerm ^^ { case _~x => TmSucc(x) }
    | "pred" ~ aTerm ^^ { case _~x => TmPred(x) }
    | "iszero" ~ aTerm ^^ { case _~x => TmIsZero(x) }
  )
}

object ParseArith extends ArithParser with App {
  import arith.Core._
  import common.Show._
  
  def parse(s: String): Term = {
    val tokens = new lexical.Scanner(s)
    phrase(term)(tokens) match {
      case Success(t,_) => t
      case t:NoSuccess => sys.error(t.toString())
    }
  }
  
  override def main(args: Array[String]): Unit = {
    args foreach (s => println(eval(parse(s))))
  }










}