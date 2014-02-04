package common

sealed trait Binding
case object NameBind extends Binding

// evaluation context

trait ContextT
case class NoContext() extends ContextT
trait ConcreteContextT extends ContextT {
  def length() : Int
  def addbinding(x:String, b:Binding) : ContextT
  def addname(x:String) : ContextT
  def index2name(i:Int) : String
  def name2index(x:String) : Int
  def getbinding(i:Int) : Binding
  def isnamebound(x:String) : Boolean
}

trait Term

trait Command
case class Eval(t:Term) extends Command
case class Bind(x:String, b:Binding) extends Command

// a Show type class
trait Show[C <: ContextT] {
  def show(ctx:C, t:Term) : String
}

object Show {
  def show[C <: ContextT](ctx:C, t:Term)(implicit shower: Show[C]): String = shower.show(ctx,t) 
}

case class NoRuleApplies(val t:Term) extends Exception("No rule applies to term: " + t)
