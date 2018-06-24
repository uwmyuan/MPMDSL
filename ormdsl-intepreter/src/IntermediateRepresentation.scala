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

case class VectorElementIR(v: VectorIR, indices: List[IndexIR]) extends ExpIR

trait VectorIR extends ExpIR {
  def apply(idx: IndexIR*) = VectorElementIR(this, idx.toList)
}

case class SumIR(idx: IndexIR, e: ExpIR) extends ExpIR

case class AExpIR(e1: ExpIR, op: AopIR, e2: ExpIR) extends ExpIR

case class PowExpIR(e: ExpIR, n: Double) extends ExpIR

trait Declaration

trait InputIR extends Declaration

trait InputSet extends InputIR

case class IndexIR(name: String, inputSet: InputSet) extends Declaration

case class DoubleSet(name: String,
                     lowerbound: ExpIR=null,
                     upperbound: ExpIR=null) extends InputSet with VectorIR

case class IntegerSet(name: String,
                      lowerbound: ExpIR=null,
                      upperbound: ExpIR=null) extends InputSet with VectorIR

trait InputVariable extends InputIR with ExpIR

case class DoubleNum(name: String,
                     lowerbound: ExpIR=null,
                     upperbound: ExpIR=null) extends InputVariable with DoubleType with VectorIR

case class IntegerNum(name: String,
                      upperbound: ExpIR=null,
                      lowerbound: ExpIR=null) extends InputVariable with IntegerType with VectorIR

trait DecisionVariable extends ExpIR with Declaration

case class IntegerDecisionVariable(name: String,
                                   lowerbound: ExpIR=null,
                                   upperbound: ExpIR=null) extends DecisionVariable with IntegerType with VectorIR

case class DoubleDecisionVariable(name: String,
                                  lowerbound: ExpIR=null,
                                  upperbound: ExpIR=null) extends DecisionVariable with DoubleType with VectorIR

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

trait ExpType

trait IntegerType extends ExpType

trait DoubleType extends ExpType

trait Qualifier

case class SetQualifier(name: String,
                        index: IndexIR,
                        lowerbound: ExpIR,
                        upperbound: ExpIR) extends Qualifier

case class CompoundQualifier(name: String,
                             list: List[SetQualifier]) extends Qualifier

//constraint
trait Constraint

case class QualifiedConstraint(name:String,
                               equation: EquationIR,
                               qualifier: Qualifier) extends Constraint

case class SimpleConstraint(name:String,
                            equation: EquationIR) extends Constraint

case class DecisionVariableConstraint(name:String,
                                      equation: EquationIR,
                                      decisionVariable: DecisionVariable) extends Constraint

//objective
trait ObjectiveIR

case class MinObjectiveIR(e: ExpIR) extends ObjectiveIR

case class MaxObjectiveIR(e: ExpIR) extends ObjectiveIR

//formula
trait Formulation

case class FormulaIR(declarations: List[Declaration],
                     objective: ObjectiveIR,
                     constraints: List[Constraint]) extends Formulation

object FormulaIR {

  /**
    * check if all decision variables are well defined
    *
    * @return
    */
  def checkDecisionVariables: Boolean = ???

  /**
    * check if all decision variables are related to the objective
    *
    * @return
    */
  def checkObjective: Boolean = ???

  /**
    * check if all constraints are related at least one decision variables
    *
    * @return
    */
  def checkConstraints: Boolean = ???


  /**
    * get all the decision variables
    *
    * @return
    */
  def getDecisionVariables: Map[String, DecisionVariable] = ???

  /**
    * get all the input variables
    *
    * @return
    */
  def getInputVariables: Map[String, InputIR] = ???

}