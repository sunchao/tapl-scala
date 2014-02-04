package arith

import common._

case object TmTrue extends Term
case object TmFalse extends Term
case class TmIf(val pred: Term, val _then: Term, val _else: Term) extends Term
case object TmZero extends Term
case class TmSucc(val num: Term) extends Term
case class TmPred(val num: Term) extends Term
case class TmIsZero(val num: Term) extends Term

object Term {
  implicit val arithshow = new Show[NoContext] {
    def show(cxt:NoContext, t:Term) : String = {
      t match {
        case TmTrue => "true"
        case TmFalse => "false"
        case TmIf(pred,_then,_else) => 
          "if " + show(cxt,pred) + " then " + show(cxt,_then) + " else " + show(cxt,_else)
        case TmZero => "0"
        case TmSucc(n) => "succ(" + show(cxt,n) + ")"
        case TmPred(n) => "pred(" + show(cxt,n) + ")"
        case TmIsZero(n) => "iszero(" + show(cxt,n) + ")"
      }
    }
  }

}
