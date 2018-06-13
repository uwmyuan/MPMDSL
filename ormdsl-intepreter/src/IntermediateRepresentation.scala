trait Qualifier
case class SetQualifier(name: String, index: Index) extends Qualifier

trait Declaration
case class InputSet(name: String,
                    index: Index,
                    lowerbound: Exp,
                    upperbound: Exp) extends Declaration
case class InputVec(name: String,
                    vecElem: VecElem) extends Declaration
case class DoubleNum(name: String,
                     upperbound: Exp,
                     lowerbound: Exp) extends Exp with Declaration
case class IntegerNum(name: String, upperbound: Exp, lowerbound: Exp) extends Exp with Declaration
case class DecisionVariableDeclaration(name: String, vecElem: VecElem, equation: Equation) extends Declaration

trait ProblemScale {
  def getDecisionVariables: Map[String, DecisionVariable]
  def getInputVariables: Map[String, InputVar]
  def getConstraints: Map[String, Constraint]
}

//constraint
trait Constraint
case class QualifiedConstraint(equation: Equation,
                               qualifier: Qualifier) extends Constraint
case class SimpleConstraint(equation: Equation) extends Constraint

//formula
case class FormulaIR(declarations: List[Declaration],
                     objective: Objective,
                     constraints: List[Constraint])