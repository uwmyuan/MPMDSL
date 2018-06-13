//operator
//arithmetic operators
trait Aop
object Plus extends Aop
object Minus extends Aop
object Times extends Aop
object Div extends Aop

//comparative operators
trait Cop
object Less extends Cop
object Greater extends Cop
object LessEq extends Cop
object GreaterEq extends Cop
object Equal extends Cop

//vector
trait Vec extends Exp {
  def apply(idx: Index*) = VecElem(this, idx.toList)
}

//index
case class Dim(n: Int, name: String)
case class Index(x: String, d: Dim)

//input variable
case class InputVar(x: String) extends Var with Vec

//decision variable
trait DecisionVariable extends Var with Vec
case class IntegerVariable(x: String) extends DecisionVariable
case class ContinuousVariable(x: String) extends DecisionVariable
case class BinaryVar(x: String) extends  DecisionVariable

//expression
trait Exp {
  def *(e: Exp) = AExp(this, Times, e)
  def +(e: Exp) = AExp(this, Plus, e)
  def -(e: Exp) = AExp(this, Minus, e)
  def /(e: Exp) = AExp(this, Div, e)
  def <(e: Exp) = Equation(this, Less, e)
  def <=(e: Exp) = Equation(this, LessEq, e)
  def >=(e: Exp) = Equation(this, GreaterEq, e)
  def >(e: Exp) = Equation(this, Greater, e)
  def ===(e: Exp) = Equation(this, Equal, e)
  def min = MinObjective(this)
  def max = MaxObjective(this)
}

case class Const(n: Double) extends Exp

object Const {
  implicit val orderingByNum: Ordering[Const] = Ordering.by(e => e.n)
}

trait Var extends Exp

case class VecElem(v: Vec, indices: List[Index]) extends Exp
case class Sum(idx: Index, e: Exp) extends Exp
case class AExp(e1: Exp, op: Aop, e2: Exp) extends Exp
case class PowExp(e: Exp, n: Double) extends Exp

//equation
case class Equation(left: Exp, op: Cop, right: Exp){
  def <=(y: Exp) = BinaryEquation(left, op, right, LessEq, y)

  def >=(y: Exp) = BinaryEquation(left, op, right, GreaterEq, y)
}

case class BinaryEquation(left: Exp, leftOp: Cop, middle: Exp, rightOp: Cop, right: Exp) {
  def toEquations: List[Equation] = {
    val e1 = Equation(left, leftOp, middle)
    val e2 = Equation(middle, rightOp, right)
    List(e1, e2)
  }
}

//objective function
trait Objective
case class MinObjective(e: Exp) extends Objective
case class MaxObjective(e: Exp) extends Objective

//model formulation
case class Formula(objective: Objective, equations: List[Equation])