

object IR2AMPL {
  def printAMPL(x: Declaration): String = x match {
    case InputSet(name: String, index: Index, lowerbound: Exp, upperbound: Exp) =>
      StringBuilder.newBuilder.append("set ")
        .append(name)
        .append(":=")
        .append(printAMPL(lowerbound))
        .append(" .. ")
        .append(printAMPL(upperbound))
        .append(";\n")
        .toString()
    case InputVec(name: String, vecElem: VecElem) =>
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
    case DecisionVariableDeclaration(name: String, vecElem: VecElem, equation: Equation) =>
      StringBuilder.newBuilder
        .append("var ")
        .append(name)
        .append(vecElem.indices.fold("") { (v1, v2) => v1 + "," + printAMPL(v2.asInstanceOf[Index]) })
        .append(printAMPL(equation.op))
        .append(printAMPL(equation.right))
        .append(";\n")
        .toString
        .replace("}sum {", ", ")
  }

  def printAMPL(x: Qualifier): String = x match {
    case SetQualifier(name, index) =>
      StringBuilder.newBuilder
        .append(name)
        .append("{")
        .append(printAMPL(index))
        .append("}")
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

  def printAMPL(x: FormulaIR): String = x match {
    case FormulaIR(declarations: List[Declaration],
    objective: Objective,
    constraints: List[Constraint]) =>
      StringBuilder.newBuilder
        .append(declarations.fold("") { (v1, v2) => v1.toString + v2.toString })
        .append(printAMPL(objective))
        .append(constraints.fold("") { (v1, v2) => v1.toString + v2.toString })
        .toString()
  }
}
