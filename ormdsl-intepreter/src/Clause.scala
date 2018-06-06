//clause
import ImplictConst.int2Const

trait Clause {
  def toConstraint: List[Equation]
}

/**
  * y=|A-B|
  * 0<A,B<c
  *
  * @param expA
  * @param expB
  */
case class AbsoluteValuesClause(expY: Exp, expA: Exp, expB: Exp, upperbound: Const) extends Clause {
  override def toConstraint: List[Equation] = {
    val d1 = BinaryVariable("d1")
    val d2 = BinaryVariable("d2")
    val zero = Const(0)
    val two = Const(2)
    val e1 = zero <= expA
    val e2 = zero <= expB
    val e3 = expA <= upperbound
    val e4 = expB <= upperbound
    val e5 = zero <= expY - (expA - expB)
    val e6 = expY - (expA - expB) <= two * upperbound * d2
    val e7 = zero <= expY - (expB - expA)
    val e8 = expY - (expB - expA) <= two * upperbound * d1
    val e9 = d1 + d2 === Const(1)
    return List(e1, e2, e3, e4, e5, e6, e7, e8, e9)
  }
}

/**
  * exp=max{forall l in list}
  *
  * @param expY
  * @param expX
  */
case class MaximalValuesClause(expY: Exp, expX: List[Exp], lowerbound: List[Const], upperbound: List[Const]) extends Clause {
  override def toConstraint: List[Equation] = {
    var eqs: List[Equation] = Nil
    for (i <- 1 to expX.length) {
      var d: List[BinaryVariable] = Nil
      d = BinaryVariable("d" + i.toString) :: d
      val e1 = lowerbound(i) <= expX(i)
      val e2 = expX(i) <= upperbound(i)
      val e3 = expY >= expX(i)
      val e4 = expY <= expX(i) + (upperbound.max - lowerbound(i)) * (Const(1) - d(i))
      val e5 = d.sum === Const(1)
      eqs :+ e1
      eqs :+ e2
      eqs :+ e3
      eqs :+ e4
      eqs :+ e5
    }
    return eqs
  }
}

/**
  * exp=min{forall l in list}
  *
  * @param expY
  * @param expX
  */
case class MinimalValuesClause(expY: Exp, expX: List[Exp], lowerbound: List[Const], upperbound: List[Const]) extends Clause {
  override def toConstraint: List[Equation] = {
    val eqs: List[Equation] = Nil
    for (i <- 1 to expX.length) {
      var d: List[BinaryVariable] = Nil
      d = BinaryVariable("d" + i.toString) :: d
      val e1 = lowerbound(i) <= expX(i)
      val e2 = expX(i) <= upperbound(i)
      val e3 = expY >= expX(i)
      val e4 = expY <= expX(i) - (upperbound.max - lowerbound(i)) * (Const(1) - d(i))
      val e5 = d.sum === Const(1)
      eqs :+ e1
      eqs :+ e2
      eqs :+ e3
      eqs :+ e4
      eqs :+ e5
    }
    return eqs
  }

  /**
    * exp=max{forall l in list}
    *
    * @param exp
    * @param expMin
    * @param expMax
    */
  case class MinimumActivityLevelClause(exp: Exp, expMin: Exp, expMax: Exp) extends Clause {
    override def toConstraint: List[Equation] = {
      val ifmake = BinaryVariable("ifmake")
      val eq1 = exp >= expMin * ifmake
      val eq2 = exp <= expMax * ifmake
      return List(eq1, eq2)
    }
  }

  /**
    * cost = FCOST* ifmake+VCOST*make
    * make>=MAKEMIN*ifmake
    * make<=MAKEMAX*ifmake
    *
    * @param make
    * @param cost
    * @param minMake
    * @param maxMake
    * @param fixedCost
    * @param variableCost
    */
  case class VariableCostLevelClause(make: Exp, cost: Exp, minMake: Exp, maxMake: Exp, fixedCost: Exp, variableCost: Exp) extends Clause {
    override def toConstraint: List[Equation] = {
      val ifmake = BinaryVariable("ifmake")
      val eq1 = cost === fixedCost * ifmake
      val eq2 = make >= minMake * ifmake
      val eq3 = make <= maxMake * ifmake
      return List(eq1, eq2, eq3)
    }
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
    * @param b
    * @param x
    */
  case class PriceBreakDiscountClause(b: List[Exp], x: List[Exp], B: List[Exp]) extends Clause {
    override def toConstraint: List[Equation] = {
      var eqs: List[Equation] = Nil
      val eq1 = b.sum
      val eq2 = b(1) <= B(1) * b(1)
      for (i <- 2 to b.length) {
        val eq3 = B(i - 1) * b(i) <= x(i) <= B(i) * b(i)
        eqs = eqs ++ eq3.toEquations
      }
      return eqs
    }
  }

  case class PriceBreakIncrementalClause(b: List[Exp], x: List[Exp], B: List[Exp]) extends Clause {
    override def toConstraint: List[Equation] = {
      val eqs: List[Equation] = Nil
      //TODO
      return eqs
    }
  }

  /**
    * y=x*d where x is continuous d is binary
    *
    * @param expY
    * @param expX continuous
    * @param expD binary
    * @param lowerbound
    * @param upperbound
    */
  case class ProductValuesClause(expY: Exp, expX: Exp, expD: Exp, lowerbound: Exp, upperbound: Exp) extends Clause {
    override def toConstraint: List[Equation] = {
      val eq1 = lowerbound <= expX <= upperbound
      val eq2 = lowerbound * expD <= expY <= upperbound * expD
      val eq3 = lowerbound * (1 - expD) <= expX - expY <= upperbound * (1 - expD)
      return eq1.toEquations ++ eq2.toEquations ++ eq3.toEquations
    }
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
  case class DisjunctionClause(l1: Const, u1: Const, l2: Const, u2: Const, exp: List[Exp]) extends Clause {
    override def toConstraint: List[Equation] = {
      val ifupper = BinaryVariable("ifupper")
      val eq1 = exp.sum <= u1 + (u2 - u1) * ifupper
      val eq2 = exp.sum >= l1 + (l2 - l1) * ifupper
      return List(eq1, eq2)
    }
  }

  /**
    * exp=list[0] or list[1] or ...
    *
    * @param expA
    * @param expB
    */
  case class LogicalOrClause(expA: Exp, expB: List[Exp]) extends Clause {
    override def toConstraint: List[Equation] = {
      val eqs: List[Equation] = Nil
      for (i <- 1 to expX.length) {
        val eq1 = expA >= expB(i)
        val eq2 = expA <= expB.sum
        eqs :+ eq1
        eqs :+ eq2
      }
      val eq3 = expA <= 1
      eqs :+ eq3
      return eqs
    }
  }


  /**
    * expY=expX[0] and expX[1] and ...
    *
    * @param expY
    * @param expX
    */
  case class LogicalAndClause(expY: Exp, expX: List[Exp]) extends Clause {
    override def toConstraint: List[Equation] = {
      val eqs: List[Equation] = Nil
      for (i <- 1 to expX.length) {
        val eq1 = expY <= expX(i)
        val eq2 = expY >= expX.sum - (Const(expX.length) - Const(1))
        eqs :+ eq1
        eqs :+ eq2
      }
      val eq3 = expY >= Const(0)
      eqs :+ eq3
      return eqs
    }
  }


  /**
    * expA=!expB
    *
    * @param expA
    * @param expB
    */
  case class LogicalNotClause(expA: Exp, expB: Exp) extends Clause {
    override def toConstraint: Equation = {
      return expA === 1 - expB
    }
  }


  abstract class LogicClause extends Clause

  /**
    * at most n of members of list are true
    *
    * @param n
    * @param list
    */
  case class AtMostClause(n: Int, list: List[BinaryVariable]) extends LogicClause {
    override def toConstraint: List[Equation] = {
      val eq = list.sum <= n
      return List(eq)
    }
  }

  /**
    * at lest n of members of list are true
    *
    * @param n
    * @param list
    */
  case class AtLeastClause(n: Int, list: List[BinaryVariable]) extends LogicClause {
    override def toConstraint: List[Equation] = List(list.sum > n)
  }

  /**
    * exactly n of members of list are true
    *
    * @param n
    * @param list
    */
  case class ExactlyClause(n: Int, list: List[BinaryVariable]) extends LogicClause {
    override def toConstraint: List[Equation] = List(list.sum === n)
  }

  /**
    * if A then B
    *
    * @param expA
    * @param expB
    */
  case class IfThenClause(expA: Exp, expB: Exp) extends LogicClause {
    override def toConstraint: List[Equation] = List(expB >= expA)
  }

  /**
    * not A
    *
    * @param expA
    */
  case class NotClause(expA: Exp) extends LogicClause {
    override def toConstraint: List[Equation] = List(expA === 1 - expA)
  }

  /**
    * if A then not B
    *
    * @param expA
    * @param expB
    */
  case class IfThenNotClause(expA: Exp, expB: Exp) extends LogicClause {
    override def toConstraint: List[Equation] = List(expA+expB<=1)
  }

  /**
    * it not A then B
    *
    * @param expA
    * @param expB
    */
  case class IfNotThenClause(expA: Exp, expB: Exp) extends LogicClause {
    override def toConstraint: List[Equation] = List(expA+expB>=1)
  }

  /**
    * A=B
    *
    * @param expA
    * @param expB
    */
  case class EqualClause(expA: Exp, expB: Exp) extends LogicClause {
    override def toConstraint: List[Equation] = List(expA===expB)
  }

  /**
    * if A then B and C...; A only if B and C
    *
    * @param expA
    * @param expB
    */
  case class IfOnlyClause(expA: Exp, expB: Exp, expC:Exp) extends LogicClause {
    override def toConstraint: List[Equation] = List(expB>=expA,expC>=expA)
  }

  /**
    * if A then B[0] or B[1] or ...
    *
    * @param expA
    * @param expB
    */
  case class IfThenOr(expA: Exp, expB: List[Exp]) extends LogicClause {
    override def toConstraint: List[Equation] = List(expB.sum>=expA)
  }

  /**
    * if B or C then A
    *
    * @param expA
    * @param expB
    */
  case class IfOrThen(expA:Exp, expB: Exp, expC:Exp) extends LogicClause {
    override def toConstraint: List[Equation] = List(expA>=expB,expA>=expC)
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
  case class OfMoreThen(m: Int, n: Int, expA: Exp, expB: List[Exp]) extends LogicClause {
    override def toConstraint: Equation = {
      return expA >= (expB.sum - m + 1) / (n - m + 1)
    }
  }
}
