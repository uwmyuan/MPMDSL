
object IR2Tex {

  def printTex(x: AopIR): String = x match {
    case PlusIR => "+"
    case MinusIR => "-"
    case TimesIR => "*"
    case DivIR => "/"
  }

  def printTex(x: CopIR): String = x match {
    case LessIR => "<"
    case GreaterIR => ">"
    case LessEqIR => "\\leq "
    case GreaterEqIR => "\\geq "
    case EqualIR => "="
  }

  def printTex(expIR: ExpIR): String = expIR match {
    case AExpIR(e1: ExpIR, op: AopIR, e2: ExpIR) =>
      printTex(e1) + printTex(op) + printTex(e2)
    case PowExpIR(e: ExpIR, n: Double) =>
      printTex(e) + "^" + n.toString
    case SumIR(idx: IndexIR, e: ExpIR) =>
      "\\sum_{" + printTexSum(idx) + "} " + printTex(e)
    case VectorElementIR(v: VectorIR, indices: List[IndexIR]) =>
      StringBuilder.newBuilder
        .append(printTex(v) + "_{")
        .append(indices.foldLeft("") { (v1, v2) => v1 + printTex(v2) })
        .append("}")
        .toString
    case ConstIR(n: Double) =>
      if (n % 1 == 0) {
        n.toInt.toString
      }
      else n.toString
    case IntegerDecisionVariable(name, lowerbound, upperbound) => name
    case DoubleDecisionVariable(name, lowerbound, upperbound) => name
    case DoubleNum(name, upperbound, lowerbound) => name
    case IntegerNum(name, upperbound, lowerbound) => name
  }

  def printTex(y: IndexIR): String = y match {
    case IndexIR(name: String, inputSet: InputSet) => name
  }

  def printTex(objective: ObjectiveIR): String = objective match {
    case MinObjectiveIR(e: ExpIR) => "\\[\\min{" + printTex(e) + "}\\]"
    case MaxObjectiveIR(e: ExpIR) => "\\[\\max{" + printTex(e) + "}\\]"
  }

  def printTex(x: FormulaIR): String = x match {
    case FormulaIR(declarations: List[Declaration],
    objective: ObjectiveIR,
    constraints: List[Constraint]) =>
      StringBuilder.newBuilder
        .append(printTex(objective))
        .append("\n")
        .append(constraints.foldLeft("") { (v1, v2) => v1 + printTex(v2) })
        .toString
        .replace("}sum {", ", ")
  }

  def printTexSum(inputSet: InputSet): String = inputSet match {
    case DoubleSet(name, lowerbound, upperbound) => "\\in " + name
    case IntegerSet(name, lowerbound, upperbound) => "\\in " + name
  }

  def printTexSum(x: IndexIR): String = x match {
    case IndexIR(x: String, inputSet: InputSet) =>
      x + printTexSum(inputSet)
  }

  def printTex(qualifier: Qualifier): String = qualifier match {
    case SetQualifier(name, index, lowerbound, upperbound) =>
      StringBuilder.newBuilder
        .append("\\forall ")
        .append(printTexSum(index))
        .toString
    case CompoundQualifier(name, list) =>
      StringBuilder.newBuilder
        .append(list.fold("")((v1, v2) => v1 +"\\forall " +printTexSum(v2.asInstanceOf[SetQualifier].index) + ","))
        .dropRight(1)
        .toString
  }

  def printTex(equation: EquationIR): String = equation match {
    case EquationIR(left: ExpIR, op: CopIR, right: ExpIR) =>
      StringBuilder.newBuilder
        .append(printTex(left))
        .append(printTex(op))
        .append(printTex(right))
        .toString
  }

  def printTex(x: Constraint): String = x match {
    case QualifiedConstraint(name, equation, qualifier) =>
      StringBuilder.newBuilder
        .append("\\[")
        .append(printTex(equation))
        .append("\\quad ")
        .append(printTex(qualifier))
        .append("\\label{" + name + "}")
        .append("\\]\n")
        .toString
    case SimpleConstraint(name, equation) =>
      StringBuilder.newBuilder
        .append("\\[")
        .append(printTex(equation))
        .append("\\label{" + name + "}")
        .append("\\]\n")
        .toString
  }
}
