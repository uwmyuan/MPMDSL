object IR2AMPL {
  def printAMPL(x: AopIR): String = x match {
    case PlusIR => "+"
    case MinusIR => "-"
    case TimesIR => "*"
    case DivIR => "/"
  }

  def printAMPL(x: CopIR): String = x match {
    case LessIR => "<"
    case GreaterIR => ">"
    case LessEqIR => "<="
    case GreaterEqIR => ">="
    case EqualIR => "="
  }

  def printAMPL(r: ExpIR): String = r match {
    case ConstIR(n) =>
      if (n % 1 == 0) {
        n.toInt.toString
      }
      else {
        n.toString
      }
    case VectorIR(v, indices) =>
      StringBuilder.newBuilder
        .append(printAMPL(v))
        .append("[")
        .append(indices.fold("") {
          (v1, v2) => v1 + "," + printAMPL(v2.asInstanceOf[IndexIR])
        }.toString.drop(1))
        .append("]")
        .toString
    case SumIR(idx, e) => "sum {" + printAMPLSum(idx) + "}" + printAMPL(e)
    case AExpIR(e1, op, e2) => printAMPL(e1) + printAMPL(op) + printAMPL(e2)
    case PowExpIR(e, n) => printAMPL(e) + "^" + n.toString
  }

  def printAMPL(y: IndexIR): String = y match {
    case IndexIR(x: String, inputSet) => x
  }

  def printAMPLSum(y: IndexIR): String = y match {
    case IndexIR(x: String, inputSet) => x + printAMPL(inputSet)
  }

  def printAMPL(x: InputVariable): String = x match {

    case VecNum(name: String, vecElem: VectorIR) =>
      StringBuilder.newBuilder
        .append("param ")
        .append(name)
        .append(printAMPL(vecElem))
        .append(";\n")
        .toString().replace("}sum {", ", ")
    case IntegerNum(name, upperbound, lowerbound) =>
      StringBuilder.newBuilder
        .append("param ")
        .append(name)
        .append(if (lowerbound != null) " >=" + printAMPL(lowerbound) else "")
        .append(if (lowerbound != null) " <=" + printAMPL(upperbound) else "")
        .append(";\n")
        .toString()
    case DoubleNum(name, upperbound, lowerbound) =>
      StringBuilder.newBuilder
        .append("param ")
        .append(name)
        .append(if (lowerbound != null) " >=" + printAMPL(lowerbound))
        .append(if (upperbound != null) " <=" + printAMPL(upperbound))
        .toString
  }

  def printAMPL(inputSet: InputSet): String = inputSet match {
    case IntegerSet(name: String, lowerbound: ExpIR, upperbound: ExpIR) =>
      StringBuilder.newBuilder.append("set ")
        .append(name)
        .append(":=")
        .append(printAMPL(lowerbound))
        .append(" .. ")
        .append(printAMPL(upperbound))
        .append(";\n")
        .toString

    case DoubleSet(name, lowerbound, upperbound) =>
      StringBuilder.newBuilder
        .append(name)
        .append(if (lowerbound != null) " >=" + printAMPL(lowerbound) else "")
        .append(if (lowerbound != null) " <=" + printAMPL(upperbound) else "")
        .toString
    case VecSet(name, lowerbound, upperbound, idx) =>
      StringBuilder.newBuilder
        .toString
  }

  def printAMPL(decisionVariable: DecisionVariable): String = decisionVariable match {
    case IntegerDecisionVariable(name: String,
    lowerbound: ExpIR,
    upperbound: ExpIR) =>
      StringBuilder.newBuilder
        .append("var ")
        .append(name)
        .append(";\n")
        .toString
        .replace("}sum {", ", ")
  }

  def printAMPL(x: Qualifier): String = x match {
    case SetQualifier(name, index, upperbound, lowerbound) =>
      StringBuilder.newBuilder
        .append(name)
        .append("{")
        .append(printAMPL(index))
        .append("}")
        .toString()
  }

  def printAMPL(x: EquationIR): String = x match {
    case EquationIR(left: Exp, op: Cop, right: Exp) =>
      StringBuilder.newBuilder
        .append("s.t. ")
        .append(printAMPL(left))
        .append(printAMPL(op))
        .append(printAMPL(right))
        .append(";")
        .toString()
  }

  def printAMPL(x: Constraint): String = x match {
    case QualifiedConstraint(equation, qualifier) =>
      StringBuilder.newBuilder
        .append("s.t. ")
        .append(printAMPL(qualifier))
        .append(printAMPL(equation))
        .append(";")
        .toString()
    case SimpleConstraint(equation) => printAMPL(equation)
  }

  def printAMPL(objectiveIR: ObjectiveIR): String = objectiveIR match {
    case MinObjective(e: ExpIR) => "minimize obj: " + printAMPL(e) + ";"
    case MaxObjective(e: ExpIR) => "maximize obj: " + printAMPL(e) + ";"
  }

  def printAMPL(x: FormulaIR): String = x match {
    case FormulaIR(declarations: List[Declaration],
    objective: ObjectiveIR,
    constraints: List[Constraint]) =>
      StringBuilder.newBuilder
        .append(declarations.fold("") { (v1, v2) => v1.toString + v2.toString })
        .append(printAMPL(objective))
        .append(constraints.fold("") { (v1, v2) => v1.toString + v2.toString })
        .toString()
  }
}
