package com.ormdsl.interpreter

// ==================== Validation Exceptions ====================

case class ValidationException(message: String) extends Exception(message)

object ValidationErrors {
  val BOUND_INVERSION = "Bound inversion: lower bound must be <= upper bound"
  val NEGATIVE_INDEX = "Negative index: array index must be >= 0"
  val BINARY_OUT_OF_RANGE = "Binary variable bounds must be in [0, 1]"
  val INTEGER_BOUND_NOT_INTEGER = "Integer variable bounds must be integers"
  val DUPLICATE_VARIABLE = "Duplicate variable name"
  val UNDEFINED_VARIABLE = "Undefined variable"
  val MIXED_TYPES_IN_EXPRESSION = "Mixed types in expression"
}

// ==================== Validation Utilities ====================

object IRValidator {
  def validateBounds(lb: ExpIR, ub: ExpIR, varName: String): Unit = {
    (lb, ub) match {
      case (ConstIR(l), ConstIR(u)) if l > u =>
        throw ValidationException(s"$varName: ${ValidationErrors.BOUND_INVERSION} (lb=$l > ub=$u)")
      case _ =>
    }
  }

  def validateNonNegativeIndex(idx: ExpIR): Unit = idx match {
    case ConstIR(n) if n < 0 =>
      throw ValidationException(s"${ValidationErrors.NEGATIVE_INDEX}: got $n")
    case _ =>
  }

  def validateBinaryBounds(lb: ExpIR, ub: ExpIR, varName: String): Unit = {
    (lb, ub) match {
      case (ConstIR(l), ConstIR(u)) if l < 0 || u > 1 =>
        throw ValidationException(s"$varName: ${ValidationErrors.BINARY_OUT_OF_RANGE} (lb=$l, ub=$u)")
      case _ =>
    }
  }

  def validateIntegerBounds(lb: ExpIR, ub: ExpIR, varName: String): Unit = {
    def isInteger(e: ExpIR): Boolean = e match {
      case ConstIR(n) => n == n.toLong
      case _ => false
    }
    if (!isInteger(lb) || !isInteger(ub)) {
      throw ValidationException(s"$varName: ${ValidationErrors.INTEGER_BOUND_NOT_INTEGER} (lb=$lb, ub=$ub)")
    }
  }

  def checkDuplicateNames(declarations: List[Declaration]): Unit = {
    val names = declarations.collect {
      case dv: DecisionVariable => dv.name
      case iv: InputVariable => iv match {
        case d: DoubleNum => d.name
        case i: IntegerNum => i.name
      }
      case s: InputSet => s.name
      case ir: IndexIR => ir.name
    }
    if (names.distinct.length != names.length) {
      val duplicates = names.groupBy(identity).filter(_._2.length > 1).keys.mkString(", ")
      throw ValidationException(s"${ValidationErrors.DUPLICATE_VARIABLE}: $duplicates")
    }
  }

  def checkUndefinedVariables(expr: ExpIR, knownVars: Set[String]): Unit = {
    val used = expr.resolve.keySet
    used.foreach { v =>
      if (!knownVars(v)) {
        throw ValidationException(s"${ValidationErrors.UNDEFINED_VARIABLE}: '$v'")
      }
    }
  }
}

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

  def resolve: Map[String, Declaration] = this match {
    case ConstIR(n) => Map()
    case VectorElementIR(v, indices) => v.resolve
    case SumIR(idx, e) => e.resolve
    case AExpIR(e1, op, e2) => e1.resolve ++ e2.resolve
    case PowExpIR(e, n) => e.resolve
    case IntegerDecisionVariable(name, lowerbound, upperbound) => Map(name -> this.asInstanceOf[IntegerDecisionVariable])
    case DoubleDecisionVariable(name, lowerbound, upperbound) => Map(name -> this.asInstanceOf[DoubleDecisionVariable])
    case DoubleNum(name, upperbound, lowerbound) => Map(name -> this.asInstanceOf[DoubleNum])
    case IntegerNum(name, upperbound, lowerbound) => Map(name -> this.asInstanceOf[IntegerNum])
    case _ => Map()
  }

}

case class ConstIR(n: Double) extends ExpIR

object ConstIR {
  implicit val orderingByNum: Ordering[ConstIR] = Ordering.by(e => e.n)
}

case class VectorElementIR(v: VectorIR, indices: List[IndexIR]) extends ExpIR {
  // Validate indices at construction time
  indices.foreach { idx =>
    idx match {
      case IndexIR(name, IntegerSet(_, lb, ub)) =>
        // Check range validity
        (lb, ub) match {
          case (ConstIR(l), ConstIR(u)) if l > u =>
            throw ValidationException(s"IndexSet '$name': ${ValidationErrors.BOUND_INVERSION} ($l > $u)")
          case _ =>
        }
      case IndexIR(name, DoubleSet(_, lb, ub)) =>
        (lb, ub) match {
          case (ConstIR(l), ConstIR(u)) if l > u =>
            throw ValidationException(s"IndexSet '$name': ${ValidationErrors.BOUND_INVERSION} ($l > $u)")
          case _ =>
        }
    }
  }
}

trait VectorIR extends ExpIR {
  def apply(idx: IndexIR*) = VectorElementIR(this, idx.toList)
}

case class SumIR(idx: IndexIR, e: ExpIR) extends ExpIR

case class AExpIR(e1: ExpIR, op: AopIR, e2: ExpIR) extends ExpIR

case class PowExpIR(e: ExpIR, n: Double) extends ExpIR

trait Declaration {
  val name: String
}

trait InputIR extends Declaration

trait InputSet extends InputIR

case class IndexIR(name: String, inputSet: InputSet) extends Declaration

case class DoubleSet(name: String,
                     lowerbound: ExpIR = null,
                     upperbound: ExpIR = null) extends InputSet with VectorIR

case class IntegerSet(name: String,
                      lowerbound: ExpIR = null,
                      upperbound: ExpIR = null) extends InputSet with VectorIR

trait InputVariable extends InputIR with ExpIR

case class DoubleNum(name: String,
                     lowerbound: ExpIR = null,
                     upperbound: ExpIR = null) extends InputVariable with DoubleType with VectorIR

case class IntegerNum(name: String,
                      upperbound: ExpIR = null,
                      lowerbound: ExpIR = null) extends InputVariable with IntegerType with VectorIR

trait DecisionVariable extends ExpIR with Declaration

case class IntegerDecisionVariable(name: String,
                                   lowerbound: ExpIR = null,
                                   upperbound: ExpIR = null) extends DecisionVariable with IntegerType with VectorIR

case class DoubleDecisionVariable(name: String,
                                  lowerbound: ExpIR = null,
                                  upperbound: ExpIR = null) extends DecisionVariable with DoubleType with VectorIR

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

case class QualifiedConstraint(name: String,
                               equation: EquationIR,
                               qualifier: Qualifier) extends Constraint

case class SimpleConstraint(name: String,
                            equation: EquationIR) extends Constraint

case class DecisionVariableConstraint(name: String,
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
                     constraints: List[Constraint]) extends Formulation {

  /**
   * get all the decision variables
   *
   * @return Map of name -> DecisionVariable
   */
  def getDecisionVariables: Map[String, DecisionVariable] = {
    declarations.collect {
      case dv: IntegerDecisionVariable => dv.name -> dv
      case dv: DoubleDecisionVariable => dv.name -> dv
    }.toMap
  }

  /**
   * get all the input variables (includes InputVariable, IndexIR, InputSet)
   *
   * @return Map of name -> InputIR
   */
  def getInputVariables: Map[String, InputIR] = {
    declarations.collect {
      case iv: InputVariable => iv match {
        case iv: DoubleNum => (iv.name, iv): (String, InputIR)
        case iv: IntegerNum => (iv.name, iv): (String, InputIR)
      }
      case set: InputSet => set match {
        case set: DoubleSet => (set.name, set): (String, InputIR)
        case set: IntegerSet => (set.name, set): (String, InputIR)
      }
    }.toMap
  }

  /**
   * get all declared variable names (for scope checking)
   */
  private def getAllDeclaredNames: Set[String] = {
    (getDecisionVariables.keySet ++ getInputVariables.keySet)
  }

  /**
   * Full validation of the entire formulation
   * @return (isValid, List of error messages)
   */
  def validate: (Boolean, List[String]) = {
    val errors = scala.collection.mutable.ListBuffer[String]()

    // 1. Check for duplicate names
    try {
      IRValidator.checkDuplicateNames(declarations)
    } catch {
      case e: ValidationException => errors += e.getMessage
    }

    // 2. Check each decision variable
    val knownVars = getAllDeclaredNames
    getDecisionVariables.foreach { case (name, dv) =>
      try {
        dv match {
          case v: IntegerDecisionVariable =>
            if (v.lowerbound != null && v.upperbound != null) {
              IRValidator.validateBounds(v.lowerbound, v.upperbound, name)
              IRValidator.validateIntegerBounds(v.lowerbound, v.upperbound, name)
            }
          case v: DoubleDecisionVariable =>
            if (v.lowerbound != null && v.upperbound != null) {
              IRValidator.validateBounds(v.lowerbound, v.upperbound, name)
            }
        }
      } catch {
        case e: ValidationException => errors += e.getMessage
      }
    }

    // 3. Check objective uses only declared variables
    try {
      val objExpr = objective match {
        case MinObjectiveIR(e) => e
        case MaxObjectiveIR(e) => e
      }
      IRValidator.checkUndefinedVariables(objExpr, knownVars)
    } catch {
      case e: ValidationException => errors += s"Objective: ${e.getMessage}"
    }

    // 4. Check each constraint
    constraints.foreach { c =>
      try {
        val eq = c match {
          case QualifiedConstraint(_, eq, _) => eq
          case SimpleConstraint(_, eq) => eq
          case DecisionVariableConstraint(_, eq, _) => eq
        }
        IRValidator.checkUndefinedVariables(eq.left, knownVars)
        IRValidator.checkUndefinedVariables(eq.right, knownVars)
        IRValidator.validateBounds(eq.left, eq.right, c.toString)
      } catch {
        case e: ValidationException => errors += s"Constraint: ${e.getMessage}"
      }
    }

    // 5. Check vector indices are non-negative
    declarations.foreach { d =>
      try {
        d match {
          case v: VectorIR => // handled in apply()
          case _ =>
        }
      } catch {
        case e: ValidationException => errors += e.getMessage
      }
    }

    (errors.isEmpty, errors.toList)
  }

  /**
   * check if all decision variables are well defined
   * Decision variables must have valid bounds (lowerbound and upperbound)
   *
   * @return true if all decision variables have valid bounds
   */
  def checkDecisionVariables: Boolean = {
    val dvs = getDecisionVariables
    dvs.forall { case (name, dv) =>
      dv match {
        case dv: IntegerDecisionVariable => dv.lowerbound != null && dv.upperbound != null
        case dv: DoubleDecisionVariable => dv.lowerbound != null && dv.upperbound != null
        case _ => false
      }
    }
  }

  /**
   * check if all decision variables are related to the objective
   * Extracts variables from objective and verifies at least one is a decision variable
   *
   * @return true if objective is related to at least one decision variable
   */
  def checkObjective: Boolean = {
    val objVars = objective match {
      case MinObjectiveIR(e) => e.resolve
      case MaxObjectiveIR(e) => e.resolve
    }
    val decVars = getDecisionVariables.keySet
    objVars.keySet.exists(decVars.contains)
  }

  /**
   * check if all constraints are related at least one decision variable
   *
   * @return true if all constraints involve at least one decision variable
   */
  def checkConstraints: Boolean = {
    val decVars = getDecisionVariables.keySet
    constraints.forall { c =>
      val constVars = c match {
        case QualifiedConstraint(_, eq, _) =>
          eq.left.resolve ++ eq.right.resolve
        case SimpleConstraint(_, eq) =>
          eq.left.resolve ++ eq.right.resolve
        case DecisionVariableConstraint(name, eq, dv) =>
          val dvMap = dv match {
            case dv: IntegerDecisionVariable => Map(dv.name -> dv)
            case dv: DoubleDecisionVariable => Map(dv.name -> dv)
            case _ => Map[String, Declaration]()
          }
          dvMap ++ eq.left.resolve ++ eq.right.resolve
        case _ => Map[String, Declaration]()
      }
      val constVarNames = constVars.keys.toSet
      constVarNames.exists(decVars.contains)
    }
  }
}