//operator
//arithmetic operators
trait Tex{
  def toTex:String
}
trait AMPL{
  def toAMPL:String
}
trait Aop extends Tex
object Plus extends Aop {
  override def toTex:String="+"
}
object Minus extends Aop{
  override  def toTex:String="-"
}
object Times extends Aop{
  override def toTex:String="*"
}
object Div extends Aop{
  override def toTex:String="/"
}

//comparative operators
trait Cop extends Tex
object Less extends Cop {
  override def toTex: String = "<"
}
object Greater extends Cop {
  override def toTex: String = ">"
}
object LessEq extends Cop {
  override def toTex: String = "\\leq"
}
object GreaterEq extends Cop {
  override def toTex: String = "\\geq"
}
object Equal extends Cop {
  override def toTex: String = "="
}

//vector
trait Vec extends Tex {
  def apply(idx: Index*) = VecElem(this, idx.toList)
}
case class InputVariable(x: String) extends Vec with Var {
  override def toTex: String = x
}

//decision variable
trait DecisionVariable extends Var

case class IntegerVariable(x: String) extends Vec with DecisionVariable {
  override def toTex: String = x
}
case class ContinuousVariable(x: String) extends Vec with DecisionVariable {
  override def toTex: String = x
}
case class BinaryVariable(x: String) extends Vec with DecisionVariable {
  override def toTex: String = x
}

//index
case class Dim(n: Int)

case class Index(x: String, d: Dim) extends Tex{
  override def toTex: String = x
}


//expression
trait Exp extends Tex {
  def * (e: Exp) = AExp(this, Times, e)
  def + (e: Exp) = AExp(this, Plus, e)
  def - (e: Exp) = AExp(this, Minus, e)
  def < (e: Exp) = Equation(this, Less, e)
  def <= (e: Exp) = Equation(this, LessEq, e)
  def > (e: Exp) = Equation(this, Greater, e)
  def === (e: Exp) = Equation(this, Equal, e)
  def min = MinObjective(this)
  def max = MaxObjective(this)
}

case class Const(n: Double) extends Exp {
  override def toTex: String = n.toString
}

trait Var extends Exp

case class InputVar(x: String) extends Var {
  override def toTex: String = x
}

case class VecElem(v: Vec, indices: List[Index]) extends Exp {
  override def toTex: String = {
    var c=""
    indices.foreach(c+=_.toTex)
    return v.toTex+"_{"+c+"}"
  }
}

case class Sum(idx: Index, e: Exp) extends Exp {
  override def toTex: String = "\\sum_{"+idx.toTex+"} "+e.toTex
}

case class AExp(e1: Exp, op: Aop, e2: Exp) extends Exp {
  override def toTex: String = e1.toTex+op.toTex+e2.toTex
}

case class PowExp(e: Exp, n: Double) extends Exp {
  override def toTex: String = e.toTex+"^"+n.toString
}

//equation
case class Equation(left: Exp, op: Cop, right: Exp){
  def toTex: String ="\\["+left.toTex+op.toTex+right.toTex+"\\]"
}

//objective function
trait Objective  extends Tex with AMPL

case class MinObjective(e: Exp) extends Objective{
  def toTex:String="\\min{"+e.toTex+"}"
  def toAMPL:String=""
}
case class MaxObjective(e: Exp) extends Objective {
  def toTex: String = "\\max{"+e.toTex+"}"
  def toAMPL: String = ""
}

//model formulation
case class Formula(objective: Objective, constraints: List[Equation])
