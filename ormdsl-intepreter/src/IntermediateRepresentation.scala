//operator
//arithmetic operators
trait AopIR
object PlusIR extends AopIR
object MinusIR extends AopIR
object TimesIR extends AopIR
object DivIR extends AopIR

//comparative operators
trait CopIR
object LessIR extends CopIR
object GreaterIR extends CopIR
object LessEqIR extends CopIR
object GreaterEqIR extends CopIR
object EqualIR extends CopIR

//expression
trait ExpIR {
  def *(e: ExpIR) = AExpIR(this, TimesIR, e)
  def +(e: ExpIR) = AExpIR(this, PlusIR, e)
  def -(e: ExpIR) = AExpIR(this, MinusIR, e)
  def /(e: ExpIR) = AExpIR(this, DivIR, e)
  def <(e: ExpIR) = EquationIR(this, LessIR, e)
  def <=(e: ExpIR) = EquationIR(this, LessEqIR, e)
  def >=(e: ExpIR) = EquationIR(this, GreaterEqIR, e)
  def >(e: ExpIR) = EquationIR(this, GreaterIR, e)
  def ===(e: ExpIR) = EquationIR(this, EqualIR, e)
  def min = MinObjectiveIR(this)
  def max = MaxObjectiveIR(this)
}

case class ConstIR(n: Double) extends ExpIR

object ConstIR {
  implicit val orderingByNum: Ordering[ConstIR] = Ordering.by(e => e.n)
}

case class VecElemIR(v: Vec, indices: List[Index]) extends ExpIR
case class SumIR(idx: IndexIR, e: ExpIR) extends ExpIR
case class AExpIR(e1: ExpIR, op: AopIR, e2: ExpIR) extends ExpIR
case class PowExpIR(e: ExpIR, n: Double) extends ExpIR

trait Variable
trait DecisionVariable extends Variable
case class InputIR(x: String) extends Variable
case class IndexIR(name: String, inputSet: InputSet)

trait Qualifier
case class SetQualifier(name: String, index: IndexIR) extends Qualifier
case class CompoundQualifier(name: String, list: List[SetQualifier]) extends Qualifier

trait Declaration
trait InputSet extends Declaration
case class DoubleSet(name: String,
                     lowerbound: Exp,
                     upperbound: Exp) extends InputSet
case class IntegerSet(name: String,
                      lowerbound: Exp,
                      upperbound: Exp) extends InputSet

trait InputNumber extends Declaration
case class InputVec(name: String,
                    vecElem: VecElem) extends InputNumber
case class DoubleNum(name: String,
                     upperbound: Exp,
                     lowerbound: Exp) extends Exp with Declaration
case class IntegerNum(name: String,
                      upperbound: Exp,
                      lowerbound: Exp) extends Exp with Declaration
case class DecisionVariableDeclaration(name: String, vecElem: VecElem, equation: EquationIR) extends Declaration

//equation
case class EquationIR(left: ExpIR, op: CopIR, right: ExpIR) {
  def <=(y: ExpIR) = BinaryEquationIR(left, op, right, LessEqIR, y)
  def >=(y: ExpIR) = BinaryEquationIR(left, op, right, GreaterEqIR, y)
}

case class BinaryEquationIR(left: ExpIR, leftOp: CopIR, middle: ExpIR, rightOp: CopIR, right: ExpIR) {
  def toEquations: List[EquationIR] = {
    val e1 = EquationIR(left, leftOp, middle)
    val e2 = EquationIR(middle, rightOp, right)
    List(e1, e2)
  }
}

trait Problem {
  def getDecisionVariables: Map[String, DecisionVariable]
  def getInputVariables: Map[String, InputIR]
}

//constraint
trait Constraint

case class QualifiedConstraint(equation: EquationIR,
                               qualifier: Qualifier) extends Constraint
case class SimpleConstraint(equation: EquationIR) extends Constraint
case class DecisionVariableConstraint(equation: EquationIR, qualifier: Qualifier) extends Constraint

//objective
trait ObjectiveIR
case class MinObjectiveIR(e: ExpIR) extends ObjectiveIR
case class MaxObjectiveIR(e: ExpIR) extends ObjectiveIR

//formula
case class FormulaIR(declarations: List[Declaration],
                     objective: ObjectiveIR,
                     constraints: List[Constraint]) {

  /**
    * get the constraints
    * @return
    */
  def getConstraints: List[Constraint] =constraints

}