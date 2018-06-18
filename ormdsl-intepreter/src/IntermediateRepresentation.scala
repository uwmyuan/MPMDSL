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
  def dvs: Map[String, DecisionVariable]

  def order: Double

  def ivs: Map[String, InputVariable]

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

case class ConstIR(n: Double) extends ExpIR {
  override def dvs: Map[String, DecisionVariable] = Map()

  override def ivs: Map[String, InputVariable] = Map()

  override def order: Double = 0
}

object ConstIR {
  implicit val orderingByNum: Ordering[ConstIR] = Ordering.by(e => e.n)
}

case class VectorIR(v: ExpIR, indices: List[Index]) extends ExpIR {
  override def dvs: Map[String, DecisionVariable] = v.dvs

  override def ivs: Map[String, InputVariable] = v.ivs

  override def order: Double = v.order
}

case class SumIR(idx: IndexIR, e: VectorIR) extends ExpIR {
  override def dvs: Map[String, DecisionVariable] = e.dvs

  override def ivs: Map[String, InputVariable] = e.ivs

  override def order: Double = e.order
}

case class AExpIR(e1: ExpIR, op: AopIR, e2: ExpIR) extends ExpIR {
  override def dvs: Map[String, DecisionVariable] = e1.dvs ++ e2.dvs

  override def order: Double = op match {
    case PlusIR => if (e1.order > e2.order) e1.order else e2.order
    case MinusIR => if (e1.order > e2.order) e1.order else e2.order
    case TimesIR => e1.order + e2.order
    case DivIR => e1.order + 1 / e2.order
  }

  override def ivs: Map[String, InputIR] = e1.ivs ++ e2.ivs
}

case class PowExpIR(e: ExpIR, n: Double) extends ExpIR {
  override def dvs: Map[String, DecisionVariable] = e.dvs

  override def order: Double = e.order * n

  override def ivs: Map[String, InputIR] = e.ivs
}

trait Declaration

trait InputIR extends Declaration

trait InputSet extends InputIR

case class IndexIR(name: String, inputSet: InputSet)

case class DoubleSet(name: String,
                     lowerbound: ExpIR,
                     upperbound: ExpIR) extends InputSet

case class IntegerSet(name: String,
                      lowerbound: ExpIR,
                      upperbound: ExpIR) extends InputSet

case class VecSet(name: String,
                  lowerbound: ExpIR,
                  upperbound: ExpIR,
                  idx: List[IndexIR]) extends InputSet

trait InputVariable extends ExpIR with InputIR {
  override def dvs: Map[String, DecisionVariable] = Map()

  override def order: Double = 0

}

case class VecNum(name: String,
                  v: VectorIR) extends InputVariable {
  override def ivs: Map[String, InputIR] = v.ivs
}

case class DoubleNum(name: String,
                     upperbound: ExpIR,
                     lowerbound: ExpIR) extends InputVariable {
  override def ivs: Map[String, InputIR] = Map(this.name -> this)
}

case class IntegerNum(name: String,
                      upperbound: ExpIR,
                      lowerbound: ExpIR) extends InputVariable {
  override def ivs: Map[String, InputIR] = Map(this.name -> this)
}

trait DecisionVariable extends ExpIR with Declaration {
  override def ivs: Map[String, InputIR] = Map()
}

case class IntegerDecisionVariable(name: String,
                                   lowerbound: ExpIR,
                                   upperbound: ExpIR) extends DecisionVariable {
  override def dvs = Map(this.name -> this)

  override def order: Double = 1


}

case class DoubleDecisionVariable(name: String,
                                  lowerbound: ExpIR,
                                  upperbound: ExpIR) extends DecisionVariable {
  override def dvs = Map(this.name -> this)

  override def order: Double = 1
}

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


//problem
trait Problem {
  def getDecisionVariables: Map[String, DecisionVariable]

  def getInputVariables: Map[String, InputIR]
}


trait Qualifier

case class SetQualifier(name: String,
                        index: IndexIR,
                        upperbound: ExpIR,
                        lowerbound: ExpIR) extends Qualifier

case class CompoundQualifier(name: String,
                             index: IndexIR,
                             list: List[SetQualifier]) extends Qualifier


//constraint
trait Constraint

case class QualifiedConstraint(equation: EquationIR,
                               qualifier: Qualifier) extends Constraint

case class SimpleConstraint(equation: EquationIR) extends Constraint

case class DecisionVariableConstraint(equation: EquationIR,
                                      qualifier: Qualifier) extends Constraint

//objective
trait ObjectiveIR

case class MinObjectiveIR(e: ExpIR) extends ObjectiveIR

case class MaxObjectiveIR(e: ExpIR) extends ObjectiveIR

//formula
trait Formulation

case class FormulaIR(declarations: List[Declaration],
                     objective: ObjectiveIR,
                     constraints: List[Constraint]) extends Formulation {

  /**
    * get the constraints
    *
    * @return
    */
  def getConstraints: List[Constraint] = constraints

}