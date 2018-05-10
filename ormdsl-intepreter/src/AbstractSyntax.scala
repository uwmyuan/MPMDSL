import Equality.Equality
import MinOrMax.MinOrMax

//operator
trait Aop

object Plus extends Aop

object Minus extends Aop

object Times extends Aop

object Div extends Aop

case class Vec(x: String)

case class Index(i: String)

//TODO in operator
//for example forall i in set I
object in

//expression
trait Exp

case class Const(n: Double) extends Exp

trait Var extends Exp

case class InputVar(x: String) extends Var
/**
  *
  * TODO how to abstract var vs decisionVar?
  *
  * @param x
  */
case class Variable(x: DecisionVariable) extends Exp

case class VecElem(v: Vec, indices: List[Index]) extends Exp

case class Sum(idx: Index, e: Exp) extends Exp

case class AExp(e1: Exp, op: Aop, e2: Exp) extends Exp

case class PowExp(e: Exp, n: Double) extends Exp

trait Collection

case class DecisionVariableCollection(decisionVariable: DecisionVariable) extends Collection

case class InputCollection(v: Var) extends Collection

//decision variable
trait DecisionVariable extends Var

case class IntegerVariable(s:String) extends DecisionVariable

case class ContinuousVariable(s:String) extends DecisionVariable

case class BinaryVariable(s:String) extends DecisionVariable


//clause
trait Clause

/**
  * y=|A|
  *
  * @param expA
  * @param expB
  */
case class AbsoluteValuesClause(expA: Exp, expB: Exp) extends Clause

/**
  * exp=max{forall l in list}
  *
  * @param exp
  * @param list
  */
case class MaximalValuesClause(exp: Exp, list: List[Exp]) extends Clause

/**
  * exp=min{forall l in list}
  *
  * @param exp
  * @param list
  */
case class MinimalValuesClause(exp: Exp, list: List[Exp]) extends Clause

/**
  * exp=max{forall l in list}
  *
  * @param exp
  * @param list
  */
case class MinimumActivityLevelClause(exp: Exp, list: List[Exp]) extends Clause

case class OrderedAlternativesClause() extends Clause

/**
  * #items <= list1[0]  -> list2[0] unit cost
  * list1[0] <= #items <=list1[1] -> list2[1]
  * ...
  *
  * @param list1
  * @param list2
  */
case class PriceBreakClause(list1: List[Exp], list2: List[Exp]) extends Clause

/**
  * y=x*d where x is continuous d is binary
  *
  * @param expY
  * @param expX
  * @param expD
  * @param l
  * @param u
  */
case class ProductValuesClause(expY: Exp, expX: Exp, expD: Exp, l: Double, u: Double) extends Clause

/**
  * either l1<=exp<=u1, or l2<=exp<=u2
  *
  * @param l1
  * @param u1
  * @param l2
  * @param u2
  * @param exp
  */
case class DisjunctionClause(l1: Double, u1: Double, l2: Double, u2: Double, exp: Exp) extends Clause

/**
  * exp=list[0] or list[1] or ...
  *
  * @param exp
  * @param list
  */
case class LogicalOrClause(exp: Exp, list: List[Exp]) extends Clause


/**
  * exp=list[0] and list[1] and ...
  *
  * @param exp
  * @param list
  */
case class LogicalAndClause(exp: Exp, list: List[Exp]) extends Clause

/**
  * expA=!expB
  *
  * @param expA
  * @param expB
  */
case class LogicalNotClause(expA: Exp, expB: Exp) extends Clause


abstract class LogicClause extends Clause

/**
  * at most n of members of list are true
  *
  * @param n
  * @param list
  */
case class AtMostClause(n: Int, list: List[BinaryVariable]) extends LogicClause

/**
  * at lest n of members of list are true
  *
  * @param n
  * @param list
  */
case class AtLeastClause(n: Int, list: List[BinaryVariable]) extends LogicClause

/**
  * exactly n of members of list are true
  *
  * @param n
  * @param list
  */
case class ExactlyClause(n: Int, list: List[BinaryVariable]) extends LogicClause

/**
  * if A then B
  *
  * @param expA
  * @param expB
  */
case class IfThenClause(expA: Exp, expB: Exp) extends LogicClause

/**
  * not A
  *
  * @param expA
  */
case class NotClause(expA: Exp) extends LogicClause

/**
  * if A then not B
  *
  * @param expA
  * @param expB
  */
case class IfThenNotClause(expA: Exp, expB: Exp) extends LogicClause

/**
  * it not A then B
  *
  * @param expA
  * @param expB
  */
case class IfNotThenClause(expA: Exp, expB: Exp) extends LogicClause

/**
  * A=B
  *
  * @param expA
  * @param expB
  */
case class EqualClause(expA: Exp, expB: Exp) extends LogicClause

/**
  * if A then B and C...; A only if B and C
  *
  * @param expA
  * @param expB
  */
case class IfOnlyClause(expA: Exp, expB: List[Exp]) extends LogicClause

/**
  * if A then B[0] or B[1] or ...
  *
  * @param expA
  * @param expB
  */
case class IfThenOr(expA: Exp, expB: List[Exp]) extends LogicClause

/**
  * if A[0] or A[1]... then B
  *
  * @param expA
  * @param expB
  */
case class IfOrThen(expA: List[Exp], expB: Exp) extends LogicClause

/**
  * if A[0] and A[1] then B
  *
  * @param expA
  * @param expB
  */
case class IfAndThen(expA: List[Exp], expB: Exp) extends LogicClause

/**
  * if m or more of elements of B then A
  *
  * @param m
  * @param expA
  * @param expB
  */
case class OfMoreThen(m: Int, expA: Exp, expB: List[Exp]) extends LogicClause


//equation
class Equation(left: Exp, right: Exp, equal: Equality) {
  def leftExpression(): Exp = left

  def rightExpression(): Exp = right

  def equality(): Equality = equal
}

//qualifier
trait Qualifier

case class CollectionQualifier(e: Exp, collection: Collection) extends Qualifier

//constraint
trait Constraint

case class QualifierConstraint(qu: Qualifier) extends Constraint

case class EquationConstraint(eq: Equation, qu: Qualifier) extends Constraint

//objective function
case class ObjectiveFunction(m: MinOrMax, exp: Exp)

//model formulation
case class formulation(objectiveFunction: ObjectiveFunction, constraints: List[Constraint])