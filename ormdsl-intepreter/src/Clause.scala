//clause
trait Clause{
  def toConstraint:List[Equation]
}

/**
  * y=|A-B|
  *0<A,B<c
  * @param expA
  * @param expB
  */
case class AbsoluteValuesClause(expY:Exp, expA: Exp, expB: Exp, c:Const) extends Clause {
  override def toConstraint: List[Equation] = {
    val d1 = BinaryVariable("d1")
    val d2 = BinaryVariable("d2")
    val zero=Const(0)
    val two=Const(2)
    val e1=zero<=expA
    val e2=zero<=expB
    val e3=expA<=c
    val e4=expB<=c
    val e5=zero<=expY-(expA-expB)
    val e6=expY-(expA-expB)<=two*c*d2
    val e7=zero<=expY-(expB-expA)
    val e8=expY-(expB-expA)<=two*c*d1
    val e9=d1+d2===Const(1)
    return List(e1,e2,e3,e4,e5,e6,e7,e8,e9)
  }
}

/**
  * exp=max{forall l in list}
  *
  * @param exp
  * @param list
  */
case class MaximalValuesClause(exp: Exp, list: List[Exp]) extends Clause {
  override def toConstraint: List[Equation] = ???
}

/**
  * exp=min{forall l in list}
  *
  * @param exp
  * @param list
  */
case class MinimalValuesClause(exp: Exp, list: List[Exp]) extends Clause {
  override def toConstraint: List[Equation] = ???
}

/**
  * exp=max{forall l in list}
  *
  * @param exp
  * @param list
  */
case class MinimumActivityLevelClause(exp: Exp, list: List[Exp]) extends Clause {
  override def toConstraint: List[Equation] = ???
}

/**
  * TODO
  */
case class OrderedAlternativesClause() extends Clause {
  override def toConstraint: List[Equation] = ???
}

/**
  * #items <= list1[0]  -> list2[0] unit cost
  * list1[0] <= #items <=list1[1] -> list2[1]
  * ...
  *
  * @param list1
  * @param list2
  */
case class PriceBreakClause(list1: List[Exp], list2: List[Exp]) extends Clause {
  override def toConstraint: List[Equation] = ???
}

/**
  * y=x*d where x is continuous d is binary
  *
  * @param expY
  * @param expX
  * @param expD
  * @param l
  * @param u
  */
case class ProductValuesClause(expY: Exp, expX: Exp, expD: Exp, l: Double, u: Double) extends Clause {
  override def toConstraint: List[Equation] = ???
}

/**
  * either l1<=exp<=u1, or l2<=exp<=u2
  *
  * @param l1
  * @param u1
  * @param l2
  * @param u2
  * @param exp
  */
case class DisjunctionClause(l1: Double, u1: Double, l2: Double, u2: Double, exp: Exp) extends Clause {
  override def toConstraint: List[Equation] = ???
}

/**
  * exp=list[0] or list[1] or ...
  *
  * @param exp
  * @param list
  */
case class LogicalOrClause(exp: Exp, list: List[Exp]) extends Clause {
  override def toConstraint: List[Equation] = ???
}


/**
  * exp=list[0] and list[1] and ...
  *
  * @param exp
  * @param list
  */
case class LogicalAndClause(exp: Exp, list: List[Exp]) extends Clause {
  override def toConstraint: List[Equation] = ???
}

/**
  * expA=!expB
  *
  * @param expA
  * @param expB
  */
case class LogicalNotClause(expA: Exp, expB: Exp) extends Clause {
  override def toConstraint: List[Equation] = ???
}


abstract class LogicClause extends Clause

/**
  * at most n of members of list are true
  *
  * @param n
  * @param list
  */
case class AtMostClause(n: Int, list: List[BinaryVariable]) extends LogicClause {
  override def toConstraint: List[Equation] = ???
}

/**
  * at lest n of members of list are true
  *
  * @param n
  * @param list
  */
case class AtLeastClause(n: Int, list: List[BinaryVariable]) extends LogicClause {
  override def toConstraint: List[Equation] = ???
}

/**
  * exactly n of members of list are true
  *
  * @param n
  * @param list
  */
case class ExactlyClause(n: Int, list: List[BinaryVariable]) extends LogicClause {
  override def toConstraint: List[Equation] = ???
}

/**
  * if A then B
  *
  * @param expA
  * @param expB
  */
case class IfThenClause(expA: Exp, expB: Exp) extends LogicClause {
  override def toConstraint: List[Equation] = ???
}

/**
  * not A
  *
  * @param expA
  */
case class NotClause(expA: Exp) extends LogicClause {
  override def toConstraint: List[Equation] = ???
}

/**
  * if A then not B
  *
  * @param expA
  * @param expB
  */
case class IfThenNotClause(expA: Exp, expB: Exp) extends LogicClause {
  override def toConstraint: List[Equation] = ???
}

/**
  * it not A then B
  *
  * @param expA
  * @param expB
  */
case class IfNotThenClause(expA: Exp, expB: Exp) extends LogicClause {
  override def toConstraint: List[Equation] = ???
}

/**
  * A=B
  *
  * @param expA
  * @param expB
  */
case class EqualClause(expA: Exp, expB: Exp) extends LogicClause {
  override def toConstraint: List[Equation] = ???
}

/**
  * if A then B and C...; A only if B and C
  *
  * @param expA
  * @param expB
  */
case class IfOnlyClause(expA: Exp, expB: List[Exp]) extends LogicClause {
  override def toConstraint: List[Equation] = ???
}

/**
  * if A then B[0] or B[1] or ...
  *
  * @param expA
  * @param expB
  */
case class IfThenOr(expA: Exp, expB: List[Exp]) extends LogicClause {
  override def toConstraint: List[Equation] = ???
}

/**
  * if A[0] or A[1]... then B
  *
  * @param expA
  * @param expB
  */
case class IfOrThen(expA: List[Exp], expB: Exp) extends LogicClause {
  override def toConstraint: List[Equation] = ???
}

/**
  * if A[0] and A[1] then B
  *
  * @param expA
  * @param expB
  */
case class IfAndThen(expA: List[Exp], expB: Exp) extends LogicClause {
  override def toConstraint: List[Equation] = ???
}

/**
  * if m or more of elements of B then A
  *
  * @param m
  * @param expA
  * @param expB
  */
case class OfMoreThen(m: Int, expA: Exp, expB: List[Exp]) extends LogicClause {
  override def toConstraint: List[Equation] = ???
}