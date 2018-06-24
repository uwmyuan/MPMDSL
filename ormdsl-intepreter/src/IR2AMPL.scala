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

  def printAMPL(expIR: ExpIR): String = expIR match {
    case ConstIR(n) =>
      if (n % 1 == 0.0) {
        n.toInt.toString
      }
      else {
        n.toString
      }
    case VectorElementIR(v, indices) =>
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
    case IntegerDecisionVariable(name, lowerbound, upperbound) => name
    case DoubleDecisionVariable(name, lowerbound, upperbound) => name
    case DoubleNum(name, upperbound, lowerbound) => name
    case IntegerNum(name, upperbound, lowerbound) => name
  }

  def printAMPL(indexIR: IndexIR): String = indexIR match {
    case IndexIR(name: String, inputSet) => name
  }

  def printAMPLSum(indexIR: IndexIR): String = indexIR match {
    case IndexIR(name: String, inputSet) => name + printAMPLSum(inputSet)
  }

  def printAMPLSum(inputSet: InputSet): String = inputSet match {
    case DoubleSet(name, lowerbound, upperbound) => " in " + name
    case IntegerSet(name, lowerbound, upperbound) => " in " + name
  }

  def printAMPL(x: Qualifier): String = x match {
    case SetQualifier(name, index, upperbound, lowerbound) =>
      StringBuilder.newBuilder
        .append("{")
        .append(printAMPLSum(index))
        .append("}")
        .toString
    case CompoundQualifier(name, list) =>
      StringBuilder.newBuilder
        .append("{")
        .append(list.fold("")((v1, v2) => v1 + printAMPLSum(v2.asInstanceOf[SetQualifier].index) + ","))
        .dropRight(1)
        .append("}")
        .toString
  }

  def printAMPL(equationIR: EquationIR): String = equationIR match {
    case EquationIR(left: ExpIR, op: CopIR, right: ExpIR) =>
      StringBuilder.newBuilder
        .append(printAMPL(left))
        .append(printAMPL(op))
        .append(printAMPL(right))
        .toString
  }

  def printAMPL(constraint: Constraint): String = constraint match {
    case QualifiedConstraint(name, equation, qualifier) =>
      StringBuilder.newBuilder
        .append("s.t. ")
        .append(name + " ")
        .append(printAMPL(qualifier))
        .append(" ")
        .append(printAMPL(equation))
        .append(";\n")
        .toString
    case SimpleConstraint(name, equation) =>
      StringBuilder.newBuilder
        .append("s.t. ")
        .append(name + " ")
        .append(printAMPL(equation))
        .append(";\n")
        .toString
  }

  def printAMPL(objectiveIR: ObjectiveIR): String = objectiveIR match {
    case MinObjectiveIR(e: ExpIR) => "minimize obj: " + printAMPL(e) + ";\n"
    case MaxObjectiveIR(e: ExpIR) => "maximize obj: " + printAMPL(e) + ";\n"
  }

  def printAMPL(declaration: Declaration): String = declaration match {
    case IntegerDecisionVariable(name, lowerbound, upperbound) =>
      StringBuilder.newBuilder
        .append("var ")
        .append(name)
        .append(if (lowerbound != null) ">=" + printAMPL(lowerbound) else "")
        .append(if (upperbound != null) "<=" + printAMPL(upperbound) else "")
        .append(", integer")
        .append(";\n")
        .toString

    case DoubleDecisionVariable(name, lowerbound, upperbound) =>
      StringBuilder.newBuilder
        .append("var ")
        .append(name)
        .append(if (lowerbound != null) ">=" + printAMPL(lowerbound) else "")
        .append(if (upperbound != null) "<=" + printAMPL(upperbound) else "")
        .append(";\n")
        .toString

    case DoubleNum(name, lowerbound, upperbound) =>
      StringBuilder.newBuilder
        .append("param ")
        .append(name)
        .append(if (lowerbound != null) ">=" + printAMPL(lowerbound) else "")
        .append(if (upperbound != null) "<=" + printAMPL(upperbound) else "")
        .append(";\n")
        .toString

    case IntegerNum(name, upperbound, lowerbound) =>
      StringBuilder.newBuilder
        .append("param ")
        .append(name)
        .append(if (lowerbound != null) ">=" + printAMPL(lowerbound) else "")
        .append(if (upperbound != null) "<=" + printAMPL(upperbound) else "")
        .append(", integer")
        .append(";\n")
        .toString

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

    case IndexIR(name: String, inputSet) => ""
  }

  def printAMPL(formulaIR: FormulaIR): String = formulaIR match {
    case FormulaIR(declarations: List[Declaration],
    objective: ObjectiveIR,
    constraints: List[Constraint]) =>
      StringBuilder.newBuilder
        .append(declarations.fold("") { (v1, v2) => v1 + printAMPL(v2.asInstanceOf[Declaration]) })
        .append(printAMPL(objective))
        .append(constraints.fold("") { (v1, v2) => v1 + printAMPL(v2.asInstanceOf[Constraint]) })
        .toString
        .replace("}sum {", ", ")
  }
}
