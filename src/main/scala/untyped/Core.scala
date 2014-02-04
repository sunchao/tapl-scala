package untyped

object Core {
	import common._
	import Syntax._
	
	def isval(ctx:Context, t:Term) = {
	  t match {
	    case TmAbs(_,_) => true
	    case _ => false
	  }
	}
	
	def eval1(ctx:Context, t:Term): Term = {
	  t match {
	    case TmApp(TmAbs(x,t12), v2) if isval(ctx,v2) =>
	      termSubstTop(v2,t12)
	    case TmApp(v1,t2) if isval(ctx,v1) =>
	      TmApp(v1, eval1(ctx,t2))
	    case TmApp(t1,t2) =>
	      TmApp(eval1(ctx,t1), t2)
	    case t => throw new NoRuleApplies(t)
	  }
	}
	
	def eval(ctx:Context, t:Term): Term = {
	  try {
	    val t1 = eval1(ctx,t)
	    eval(ctx,t1)
	  } catch {
	    case NoRuleApplies(t) => t
	  }
	}
}