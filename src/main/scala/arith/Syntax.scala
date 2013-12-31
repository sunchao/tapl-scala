package arith

import common._

sealed trait Term
case object TmTrue extends Term
case object TmFalse extends Term
case class TmIf(val pred: Term, val then: Term, val _else: Term) extends Term
case object TmZero extends Term
case class TmSucc(val num: Term) extends Term
case class TmPred(val num: Term) extends Term
case class TmIsZero(val num: Term) extends Term

sealed trait Command
case class Eval(val term: Term) extends Command

object Term {
  implicit val arithshow = new Show[Term] {
    def show(t:Term) : String = {
      t match {
        case TmTrue => "true"
        case TmFalse => "false"
        case TmIf(pred,then,_else) => "if " + show(pred) + " then " + show(then) + " else " + show(_else)
        case TmZero => "0"
        case TmSucc(n) => "succ(" + show(n) + ")"
        case TmPred(n) => "pred(" + show(n) + ")"
        case TmIsZero(n) => "iszero(" + show(n) + ")"
      }
    }
  }

}
