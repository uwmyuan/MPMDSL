object AST2Tex {
  /**
    * https://en.wikipedia.org/wiki/TeX
    */
  def printTex(x: Exp): String = x match {
    case AExp(e1: Exp, op: Aop, e2: Exp) =>
      printTex(e1) + printTex(op) + printTex(e2)
    case PowExp(e: Exp, n: Double) =>
      printTex(e) + "^" + n.toString
    case Sum(idx: Index, e: Exp) =>
      "\\sum_{" + printTexSum(idx) + "} " + printTex(e)
    case VecElem(v: Vec, indices: List[Index]) =>
      StringBuilder.newBuilder
        .append(printTex(v) + "_{")
        .append(indices.foldLeft("") { (v1, v2) => v1 + printTex(v2)})
        .append("}")
        .toString
    case Const(n: Double) =>
      if (n % 1 == 0) {
        n.toInt.toString
      }
      else n.toString
    case BinaryVar(x: String) => x
    case ContinuousVariable(x: String) => x
    case IntegerVariable(x: String) => x
    case InputVar(x: String) => x
  }

  def printTex(x: Aop): String = x match {
    case Plus => "+"
    case Minus => "-"
    case Times => "*"
    case Div => "/"
  }

  def printTex(x: Cop): String = x match {
    case Less => "<"
    case Greater => ">"
    case LessEq => "\\leq "
    case GreaterEq => "\\geq "
    case Equal => "="
  }

  def printTex(y: Index): String = y match {
    case Index(x: String, d: Dim) => x
  }

  def printTex(x: Dim): String = x match {
    case Dim(n: Int, name: String) => "\\in " + name
  }

  def printTexSum(x: Dim): String = x match {
    case Dim(n: Int, name: String) => "\\in " + name
  }

  def printTexSum(x: Index): String = x match {
    case Index(x: String, d: Dim) =>
      x + printTexSum(d)
  }

  def printTex(x: Equation): String = x match {
    case Equation(left: Exp, op: Cop, right: Exp) =>
      StringBuilder.newBuilder
        .append("\\[")
        .append(printTex(left))
        .append(printTex(op))
        .append(printTex(right))
        .append("\\]")
        .toString()
  }

  def printTex(x: Objective): String = x match {
    case MinObjective(e: Exp) => "\\[\\min{" + printTex(e) + "}\\]"
    case MaxObjective(e: Exp) => "\\[\\max{" + printTex(e) + "}\\]"
  }

  def printTex(x: Formula): String = x match {
    case Formula(objective: Objective, equations: List[Equation]) =>
      StringBuilder.newBuilder
        .append(printTex(objective))
        .append("\n")
        .append(equations.foldLeft("") {
          (v1, v2) => v1 + printTex(v2)+"\n"
        })
        .toString()
  }


}


