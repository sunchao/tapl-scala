package untyped

import common._

// `idx` is the de Bruijn index, and `total` is the total length
// of context in which the variable occurs. 
case class TmVar(val idx:Int, val total:Int) extends Term
case class TmAbs(val s:String, val t:Term) extends Term
case class TmApp(val t1:Term, val t2:Term) extends Term

class Context(val ctx: List[(String,Binding)]) extends ContextT {
  def length() : Int = ctx.length
  def addbinding(x:String, b:Binding) : Context = new Context((x,b)::ctx)
  def addname(x:String) : Context = addbinding(x,NameBind)

  def isnamebound(x:String) = ctx.exists(_._1 == x)

  def pickfreshname(x:String) : (Context,String) =
    if (isnamebound(x)) pickfreshname(x+"'")
    else (addbinding(x,NameBind), x)

  private def get(i:Int) : (String,Binding) =
    if (i >= length())
      throw new Exception("variable lookup failure: offset " + i + ", ctx size: " + length())
    else ctx(i);
  
  def index2name(i: Int) : String = get(i)._1
    
  def getbinding(i:Int) : Binding = get(i)._2

  def name2index(name: String) : Int = {
    val idx = ctx.indexWhere(_._1 == name)
    if (idx < 0) throw new Exception("identifier " + name + " is unbound.")
    idx
  }
}

object Context {
  def apply() : Context = new Context(List())
}

// Shifting
object Syntax {
	def termShift(d:Int, t:Term): Term = {
		def walk(c:Int,t:Term): Term = {
		  t match {
		    case TmVar(x,n) => if (x >= c) TmVar(x+d,n+d) else TmVar(x,n+d)
		    case TmAbs(x,t1) => TmAbs(x, walk(c+1,t1))
		    case TmApp(t1,t2) => TmApp(walk(c,t1), walk(c,t2))
		  }
		}
		walk(0,t)
	}
	
	def termSubst(j:Int, s:Term, t:Term): Term = {
	  def walk(c:Int,t:Term): Term = {
	    t match {
	      case TmVar(x,n) => if (x == j+c) termShift(c,s) else TmVar(x,n)
	      case TmAbs(x,t1) => TmAbs(x,walk(c+1,t))
	      case TmApp(t1,t2) => TmApp(walk(c,t1),walk(c,t2))
	    }
	  }
	  walk(0,t)
	}
	
	def termSubstTop(s:Term, t:Term): Term = termShift(-1,termSubst(0,termShift(1,s),t))
}

object Term {
  implicit val untypedShow = new Show[Context] {
    def show(ctx:Context, t:Term) : String = {
      t match {
        case TmAbs(x, t1) =>
        	val (ctx1, x1) = ctx.pickfreshname(x)
        	"(lambda " + x1 + ". " + show(ctx,t1) + ")"
        case TmApp(t1,t2) =>
        	"(" + show(ctx,t1) + " " + show(ctx,t2) + ")"
        case TmVar(x,n) =>
        	if (ctx.length == n) ctx.index2name(x)
        	else "[bad index]"
      }    
    }
  }
}