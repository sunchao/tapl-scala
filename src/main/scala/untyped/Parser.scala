package untyped

import common._
import scala.util.parsing.combinator.syntactical.StandardTokenParsers

class Parser extends StandardTokenParsers {
  lexical.reserved ++= List("lambda")
  lexical.delimiters ++= List("(",")",";")
  
  type TopLevel[A] = Context => (A, Context)
  type Res[A] = Context => A

  def lcid : Parser[String] = ident ^? { case id if id.charAt(0).isLower => id }
  def ucid : Parser[String] = ident ^? { case id if id.charAt(0).isUpper => id }
  
  def term : Parser[Res[Term]] = (
      appTerm
    | ("lambda" ~> lcid) ~ ("." ~> term) ^^ {
        case v ~ t => cxt : Context => TmAbs(v, t(cxt.addname(v)))
    }
    | ("lambda" ~> "_" ~> "." ~> term) ^^ {
      case t => cxt => TmAbs("_", t(cxt.addname("_")))
    }
  ) 

  def aTerm : Parser[Res[Term]] = (
      "(" ~> term <~ ")"
    | lcid ^^ { t => cxt => TmVar(cxt.name2index(t), cxt.length) }
  )
  
  def appTerm : Parser[Res[Term]] = (
      aTerm
    | (appTerm ~ aTerm) ^^ { case t1 ~ t2 => cxt =>
        TmApp(t1(cxt),t2(cxt))
      }
  )
  
  def command : Parser[TopLevel[Command]] = (
      term ^^ { t => cxt => val t1 = t(cxt); (Eval(t1),cxt)}
  )
  
  def toplevel: Parser[TopLevel[List[Command]]] = (
    ((command <~ ";") ~ toplevel) ^^ { case c ~ top => cxt =>
      val (cmd, cxt1) = c(cxt)
      val (cmds, cxt2) = top(cxt1)
      (cmd::cmds, cxt2)
    }
  )
  
}
