object AST2AMPL {
  /**
    * https://en.wikipedia.org/wiki/AMPL
    */

  def printAMPL(x: Aop): String = x match {
    case Plus => "+"
    case Minus => "-"
    case Times => "*"
    case Div => "/"
  }

  def printAMPL(x: Cop): String = x match {
    case Less => "<"
    case Greater => ">"
    case LessEq => "<="
    case GreaterEq => ">="
    case Equal => "="
  }

  def printAMPL(x: Objective): String = x match {
    case MinObjective(e: Exp) => "minimize obj: " + printAMPL(e) + ";"
    case MaxObjective(e: Exp) => "maximize obj: " + printAMPL(e) + ";"
  }

  def printAMPL(x: Formula): String = x match {
    case Formula(objective: Objective, equations: List[Equation]) =>
      StringBuilder.newBuilder
        .append(printAMPL(objective))
        .append("\n")
        .append(equations.fold("") { (v1, v2) => v1 + printAMPL(v2.asInstanceOf[Equation])+"\n" })
        .toString()
        .replace("}sum {", ", ")
  }

  def printAMPL(y: Index): String = y match {
    case Index(x: String, d: Dim) => x
  }

  def printAMPLSum(y: Index): String = y match {
    case Index(x: String, d: Dim) => x + printAMPLSum(d)
  }

  def printAMPLSum(x: Dim): String = x match {
    case Dim(n: Int, name: String) => " in " + name
  }

  def printAMPL(x: Equation): String = x match {
    case Equation(left: Exp, op: Cop, right: Exp) =>
      StringBuilder.newBuilder
        .append("s.t. ")
        .append(printAMPL(left))
        .append(printAMPL(op))
        .append(printAMPL(right))
        .append(";")
        .toString()
  }

  def printAMPL(x: Exp): String = x match {

    case Const(n) =>
      if (n % 1 == 0) {
        n.toInt.toString
      }
      else {
        n.toString
      }
    case VecElem(v, indices) =>
      StringBuilder.newBuilder
        .append(printAMPL(v))
        .append("[")
        .append(indices.fold("") {
          (v1, v2) => v1 + "," + printAMPL(v2.asInstanceOf[Index])
        }.toString.drop(1))
        .append("]")
        .toString()
    case Sum(idx, e) => "sum {" + printAMPLSum(idx) + "}" + printAMPL(e)
    case AExp(e1, op, e2) => printAMPL(e1) + printAMPL(op) + printAMPL(e2)
    case PowExp(e, n) => printAMPL(e) + "^" + n.toString
    case InputVar(y) => y
    case IntegerVariable(y) => y
    case ContinuousVariable(y) => y
    case BinaryVar(y) => y
  }


}