
/**
  * https://en.wikipedia.org/wiki/TeX
  */
trait Tex {
  def toTex: String
}

/**
  * https://en.wikipedia.org/wiki/AMPL
  */
trait AMPL {
  def toAMPL: String
}

trait TexSum {
  def toTexSum: String
}

trait AMPLSum {
  def toAMPLSum: String
}

//operator
//arithmetic operators
trait Aop extends Tex with AMPL

object Plus extends Aop {
  override def toTex: String = "+"

  override def toAMPL: String = "+"
}

object Minus extends Aop {
  override def toTex: String = "-"

  override def toAMPL: String = "-"
}

object Times extends Aop {
  override def toTex: String = "*"

  override def toAMPL: String = "*"
}

object Div extends Aop {
  override def toTex: String = "/"

  override def toAMPL: String = "/"
}

//comparative operators
trait Cop extends Tex with AMPL

object Less extends Cop {
  override def toTex: String = "<"

  override def toAMPL: String = "<"
}

object Greater extends Cop {
  override def toTex: String = ">"

  override def toAMPL: String = ">"
}

object LessEq extends Cop {
  override def toTex: String = "\\leq "

  override def toAMPL: String = "<="
}

object GreaterEq extends Cop {
  override def toTex: String = "\\geq "

  override def toAMPL: String = ">="
}

object Equal extends Cop {
  override def toTex: String = "="

  override def toAMPL: String = "="
}

//vector
trait Vec extends AMPL with Tex with Exp {
  def apply(idx: Index*) = VecElem(this, idx.toList)
}

case class InputVariable(x: String) extends Vec with Var {
  override def toTex: String = x

  override def toAMPL: String = x
}

//decision variable
trait DecisionVariable extends Var

case class IntegerVariable(x: String) extends Vec with DecisionVariable {
  override def toTex: String = x

  override def toAMPL: String = x
}

case class ContinuousVariable(x: String) extends Vec with DecisionVariable {
  override def toTex: String = x

  override def toAMPL: String = x
}

case class BinaryVariable(x: String) extends Vec with DecisionVariable {
  override def toTex: String = x

  override def toAMPL: String = x
}

//index
case class Dim(n: Int, name: String) extends TexSum with AMPLSum {
  def toAMPLSum: String = {
    return " in " + name
  }

  def toTexSum: String = {
    return "\\in " + name
  }
}

case class Index(x: String, d: Dim) extends Tex with AMPL with TexSum with AMPLSum {
  override def toTex: String = x

  def toTexSum: String = x + d.toTexSum

  def toAMPLSum: String = x + d.toAMPLSum

  override def toAMPL: String = x
}


//expression
trait Exp extends Tex with AMPL {

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

case class Const(n: Double) extends Exp {
  override def toTex: String = {
    if (n % 1 == 0) {
      return n.toInt.toString
    }
    else {
      return n.toString
    }
  }

  override def toAMPL: String = {
    if (n % 1 == 0) {
      return n.toInt.toString
    }
    else {
      return n.toString
    }
  }
}

trait Var extends Exp

case class InputVar(x: String) extends Var {
  override def toTex: String = x

  override def toAMPL: String = x
}

case class VecElem(v: Vec, indices: List[Index]) extends Exp {
  override def toTex: String = {
    var c = ""
    indices.foreach(c += _.toTex)
    return v.toTex + "_{" + c + "}"
  }

  override def toAMPL: String = {
    var c = ""
    indices.foreach(c += _.toAMPL + ",")
    c = c.dropRight(1)
    return v.toTex + "[" + c + "]"
  }
}

case class Sum(idx: Index, e: Exp) extends Exp {
  override def toTex: String = "\\sum_{" + idx.toTexSum + "} " + e.toTex

  override def toAMPL: String = "sum {" + idx.toAMPLSum + "}" + e.toAMPL
}

case class AExp(e1: Exp, op: Aop, e2: Exp) extends Exp {
  override def toTex: String = e1.toTex + op.toTex + e2.toTex

  override def toAMPL: String = e1.toAMPL + op.toAMPL + e2.toAMPL
}

case class PowExp(e: Exp, n: Double) extends Exp {
  override def toTex: String = e.toTex + "^" + n.toString

  override def toAMPL: String = e.toAMPL + "^" + n.toString
}

//equation
case class Equation(left: Exp, op: Cop, right: Exp) extends Tex with AMPL {
  def toTex: String = "\\[" + left.toTex + op.toTex + right.toTex + "\\]"

  def <=(y: Exp) = BinaryEquation(left, op, right, LessEq, y)

  override def toAMPL: String = "s.t. " + left.toAMPL + op.toAMPL + right.toAMPL + ";"
}

case class BinaryEquation(left: Exp, leftOp: Cop, middle: Exp, rightOp: Cop, right: Exp) extends {
  def toEquations: List[Equation] = {
    val e1 = Equation(left, leftOp, middle)
    val e2 = Equation(middle, rightOp, right)
    return List(e1, e2)
  }
}

//objective function
trait Objective extends Tex with AMPL

case class MinObjective(e: Exp) extends Objective {
  def toTex: String = "\\[\\min{" + e.toTex + "}\\]"

  def toAMPL: String = "minimize obj: " + e.toAMPL + ";"
}

case class MaxObjective(e: Exp) extends Objective {
  def toTex: String = "\\[\\max{" + e.toTex + "}\\]"

  def toAMPL: String = "maximize obj: " + e.toAMPL + ";"
}

//model formulation
case class Formula(objective: Objective, constraints: List[Equation])
