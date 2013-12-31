package arith

object Core {
  case class NoRuleApplies(val t:Term) extends Exception("No rule applies to term: " + t)
  
  def isNumericVal(t: Term): Boolean = t match {
    case TmZero => true
    case TmSucc(t1) => isNumericVal(t1)
    case _ => false
  } 
  
  def isVal(t: Term): Boolean = t match {
    case TmTrue => true
    case TmFalse => true
    case t if isNumericVal(t) => true
    case _ => false
  }
  
  def eval1(t: Term): Term = t match {
    case TmIf(TmTrue,t2,t3) => t2
    case TmIf(TmFalse,t2,t3) => t3
    case TmIf(t1,t2,t3) => TmIf(eval1(t1),t2,t3)
    case TmSucc(t1) => TmSucc(eval1(t1))
    case TmPred(TmSucc(t1)) if isNumericVal(t1) => t1
    case TmPred(t1) => TmPred(eval1(t1))
    case TmIsZero(TmZero) => TmTrue
    case TmIsZero(TmSucc(t1)) if isNumericVal(t1) => TmFalse
    case TmIsZero(t1) => TmIsZero(eval1(t1))
    case t => throw new NoRuleApplies(t)
  }
  
  def eval(t: Term): Term = try {
    val t1 = eval1(t)
    eval(t1)
  } catch {
    case NoRuleApplies(_) => t
  }
}
