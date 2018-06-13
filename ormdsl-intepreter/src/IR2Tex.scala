import AST2Tex.printTexSum

object IR2Tex {
  def printTex(x: FormulaIR): String = x match {
    case FormulaIR(declarations: List[Declaration],
    objective: Objective,
    constraints: List[Constraint]) =>
      StringBuilder.newBuilder
        .append(printTex(objective))
        .append("\n")
        .append(constraints.foldLeft("") { (v1, v2) => v1 + printTex(v2) })
        .toString()
        .replace("}sum {", ", ")
  }

  def printTex(qualifier: Qualifier): String = qualifier match {
    case SetQualifier(name, index) =>
      StringBuilder.newBuilder
        .append("\\forall")
        .append(printTexSum(index))
        .append(name)
        .toString
  }

  def printTex(x: Constraint): String = x match {
    case QualifiedConstraint(equation, qualifier) =>
      StringBuilder.newBuilder
        .append("\\[")
        .append(printTex(equation))
        .append("\\quad")
        .append(printTex(qualifier))
        .append("\\]")
        .toString()
    case SimpleConstraint(equation) => printTex(equation)
  }
}
