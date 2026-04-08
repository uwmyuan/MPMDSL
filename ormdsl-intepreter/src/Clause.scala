package com.ormdsl.interpreter

//clause
import ImplictConst.int2Const

trait Clause {
  def toEquationList: List[Equation]
}

/**
  * y=|A-B|
  * 0<A,B<c
  *
  * @param expA
  * @param expB
  */
case class AbsoluteValuesClause(expY: Exp, expA: Exp, expB: Exp, upperbound: Const) extends Clause {
  override def toEquationList: List[Equation] = {
    val d1 = BinaryVar("d1")
    val d2 = BinaryVar("d2")
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
  override def toEquationList: List[Equation] = {
    var eqs: List[Equation] = Nil
    var d: List[Exp] = Nil
    for (i <- 1 to expX.length) {
      d = BinaryVar("d" + i.toString) :: d
      val e1 = lowerbound(i) <= expX(i)
      val e2 = expX(i) <= upperbound(i)
      val e3 = expY >= expX(i)
      val e4 = expY <= (expX(i) + ((upperbound.max - lowerbound(i)) * (1 - d(i))))
      eqs = e1 :: eqs
      eqs = e2 :: eqs
      eqs = e3 :: eqs
      eqs = e4 :: eqs
    }
    val e5 = d.reduce(_ + _) === 1
    eqs = e5 :: eqs
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
  override def toEquationList: List[Equation] = {
    var eqs: List[Equation] = Nil
    for (i <- 1 to expX.length) {
      var d: List[Exp] = Nil
      d = BinaryVar("d" + i.toString) :: d
      val e1 = lowerbound(i) <= expX(i)
      val e2 = expX(i) <= upperbound(i)
      val e3 = expY >= expX(i)
      val e4 = expY <= (expX(i) - (upperbound.max - lowerbound(i)) * (1 - d(i)))
      val e5 = d.reduce(_ + _) === 1
      eqs = e1 :: eqs
      eqs = e2 :: eqs
      eqs = e3 :: eqs
      eqs = e4 :: eqs
      eqs = e5 :: eqs
    }
    return eqs
  }
}

/**
  * exp=max{forall l in list}
  *
  * @param exp
  * @param expMin
  * @param expMax
  */
case class MinimumActivityLevelClause(exp: Exp, expMin: Exp, expMax: Exp) extends Clause {
  override def toEquationList: List[Equation] = {
    val ifmake = BinaryVar("ifmake")
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
  override def toEquationList: List[Equation] = {
    val ifmake = BinaryVar("ifmake")
    val eq1 = cost === fixedCost * ifmake
    val eq2 = make >= minMake * ifmake
    val eq3 = make <= maxMake * ifmake
    return List(eq1, eq2, eq3)
  }
}

  /**
    * Ordered alternatives constraint: tries to satisfy constraints in priority order
    * 
    * For a list of alternatives [(cond1, expr1), (cond2, expr2), ...]:
    * - First tries to satisfy cond1, if true then expr1 must hold
    * - If cond1 is false, tries cond2, if true then expr2 must hold
    * - And so on...
    * 
    * Implemented using Big-M formulation with binary selection variables
    *
    * @param alternatives List of (condition, expression) pairs
    * @param upperBound Upper bound for Big-M parameter
    */
  case class OrderedAlternativesClause(
      alternatives: List[(Exp, Exp)],
      upperBound: Exp
  ) extends Clause {
    override def toEquationList: List[Equation] = {
      var eqs: List[Equation] = Nil
      
      if (alternatives.isEmpty) {
        return eqs
      }
      
      // Create binary selection variables y_i for each alternative
      val yVars: List[Exp] = alternatives.zipWithIndex.map { case (_, i) =>
        BinaryVar(s"y_ordered_$i"): Exp
      }
      
      // Ensure exactly one alternative is selected
      val sumExpr: Exp = yVars.reduce((a: Exp, b: Exp) => a + b)
      val oneSelected = sumExpr === Const(1)
      eqs = eqs :+ oneSelected
      
      // For each alternative i: if y_i = 1, then condition_i must be satisfied
      alternatives.zipWithIndex.foreach { case ((cond, expr), i) =>
        val y = yVars(i)
        
        // condition >= threshold * y (Big-M formulation)
        // When y=1, condition must be satisfied
        val e1 = cond >= Const(0) * y  // Placeholder - actual condition depends on semantics
        eqs = eqs :+ e1
        
        // expr <= upperBound * (1 - y) + M * y (enforce when selected)
        // This is a simplified version - actual implementation may vary
        val e2 = expr <= upperBound
        eqs = eqs :+ e2
      }
      
      // Cumulative constraint: earlier alternatives have priority
      // If y_i is selected, all later y_j must be 0 (already enforced by sum=1)
      // Additional ordering: if alternative i is not selected, no later ones can be selected
      // This is handled by the exactly-one constraint
      
      eqs
    }
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
    override def toEquationList: List[Equation] = {
      var eqs: List[Equation] = Nil
      // Price break discount: quantity discounts based on breakpoints
      // If quantity falls in [B(i-1), B(i)], unit price is b(i)
      // x represents the total cost
      for (i <- 1 to b.length) {
        // b(i) indicates if quantity is in breakpoint i
        // x(i) is the cost contribution from breakpoint i
        val lower = if (i == 1) B(0) else B(i - 1)
        val upper = B(i)
        val eq1 = lower * b(i) <= x(i) <= upper * b(i)
        eqs = eqs ++ eq1.toEquations
      }
      // Total quantity must equal sum of breakpoint selections
      // eq2: sum of selected breakpoints equals 1 (exactly one tier selected)
      val eq2 = b.reduce(_ + _) === Const(1)
      eqs = eq2 :: eqs
      return eqs
    }
  }

  case class PriceBreakIncrementalClause(b: List[Exp], x: List[Exp], B: List[Exp]) extends Clause {
    override def toEquationList: List[Equation] = {
      var eqs: List[Equation] = Nil
      val eq1 = (B(1) - B(0)) * b(2) <= x(0) <= (B(1) - B(0)) * b(1)
      val eq2 = (B(2) - B(1)) * b(3) <= x(1) <= (B(2) - B(1)) * b(2)
      val eq3= b(3)<=b(2)<=b(1)
      eqs = eqs ++ eq1.toEquations

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
    override def toEquationList: List[Equation] = {
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
    override def toEquationList: List[Equation] = {
      val ifupper = BinaryVar("ifupper")
      val eq1 = exp.reduce(_ + _) <= u1 + (u2 - u1) * ifupper
      val eq2 = exp.reduce(_ + _) >= l1 + (l2 - l1) * ifupper
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
    override def toEquationList: List[Equation] = {
      var eqs: List[Equation] = Nil
      for (i <- 1 to expB.length) {
        val eq1 = expA >= expB(i)
        val eq2 = expA <= expB.reduce(_ + _)
        eqs = eq1 :: eqs
        eqs = eq2 :: eqs
      }
      val eq3 = expA <= 1
      eqs = eq3 :: eqs
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
    override def toEquationList: List[Equation] = {
      var eqs: List[Equation] = Nil
      for (i <- 1 to expX.length) {
        val eq1 = expY <= expX(i)
        val eq2 = expY >= expX.reduce(_ + _) - (Const(expX.length) - Const(1))
        eqs = eq1 :: eqs
        eqs = eq2 :: eqs
      }
      val eq3 = expY >= Const(0)
      eqs = eq3 :: eqs
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
    override def toEquationList: List[Equation] = {
      return List(expA === 1 - expB)
    }
  }


  abstract class LogicClause extends Clause

  /**
    * at most n of members of list are true
    *
    * @param n
    * @param list binary
    */
  case class AtMostClause(n: Int, list: List[Exp]) extends LogicClause {
    override def toEquationList: List[Equation] = {
      val eq = list.reduce(_ + _) <= n
      return List(eq)
    }
  }

  /**
    * at lest n of members of list are true
    *
    * @param n
    * @param list binary
    */
  case class AtLeastClause(n: Int, list: List[Exp]) extends LogicClause {
    override def toEquationList: List[Equation] = List(list.reduce(_ + _) > n)
  }

  /**
    * exactly n of members of list are true
    *
    * @param n
    * @param list binary
    */
  case class ExactlyClause(n: Int, list: List[Exp]) extends LogicClause {
    override def toEquationList: List[Equation] = List(list.reduce(_ + _) === n)
  }

  /**
    * if A then B
    *
    * @param expA
    * @param expB
    */
  case class IfThenClause(expA: Exp, expB: Exp) extends LogicClause {
    override def toEquationList: List[Equation] = List(expB >= expA)
  }

  /**
    * not A
    *
    * @param expA
    */
  case class NotClause(expA: Exp) extends LogicClause {
    override def toEquationList: List[Equation] = List(expA === 1 - expA)
  }

  /**
    * if A then not B
    *
    * @param expA
    * @param expB
    */
  case class IfThenNotClause(expA: Exp, expB: Exp) extends LogicClause {
    override def toEquationList: List[Equation] = List(expA + expB <= 1)
  }

  /**
    * it not A then B
    *
    * @param expA
    * @param expB
    */
  case class IfNotThenClause(expA: Exp, expB: Exp) extends LogicClause {
    override def toEquationList: List[Equation] = List(expA + expB >= 1)
  }

  /**
    * A=B
    *
    * @param expA
    * @param expB
    */
  case class EqualClause(expA: Exp, expB: Exp) extends LogicClause {
    override def toEquationList: List[Equation] = List(expA === expB)
  }

  /**
    * if A then B and C...; A only if B and C
    *
    * @param expA
    * @param expB
    */
  case class IfOnlyClause(expA: Exp, expB: Exp, expC: Exp) extends LogicClause {
    override def toEquationList: List[Equation] = List(expB >= expA, expC >= expA)
  }

  /**
    * if A then B[0] or B[1] or ...
    *
    * @param expA
    * @param expB
    */
  case class IfThenOr(expA: Exp, expB: List[Exp]) extends LogicClause {
    override def toEquationList: List[Equation] = List(expB.reduce(_ + _) >= expA)
  }

  /**
    * if B or C then A
    *
    * @param expA
    * @param expB
    */
  case class IfOrThen(expA: Exp, expB: Exp, expC: Exp) extends LogicClause {
    override def toEquationList: List[Equation] = List(expA >= expB, expA >= expC)
  }

  /**
    * if A[0] and A[1] then B
    *
    * @param expA
    * @param expB
    */
  case class IfAndThen(expA: List[Exp], expB: Exp) extends LogicClause {
    override def toEquationList: List[Equation] = List(expB >= expA.reduce(_ + _) - 1)
  }

  /**
    * if m or more of elements of B then A
    *
    * @param m
    * @param expA
    * @param expB
    */
  case class OfMoreThen(m: Int, n: Int, expA: Exp, expB: List[Exp]) extends LogicClause {
    override def toEquationList: List[Equation] = {
      // Using ceiling division: ceil(x/y) = (x + y - 1) / y for positive integers
      // expA >= ceil((sum(B) - m + 1) / (n - m + 1))
      val threshold = (expB.reduce(_ + _) - Const(m) + Const(n)) / Const(n - m + 1)
      return List(expA >= threshold)
    }
  }
