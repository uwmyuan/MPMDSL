
trait Qualifier extends AMPL with Tex

case class SetQualifier(name: String, index: Index) extends Qualifier {
  override def toAMPL: String = {
    val c = StringBuilder.newBuilder
    c.append(name)
    c.append("{")
    c.append(index.toAMPLSum)
    c.append("}")
    return c.toString()
  }

  override def toTex: String = {
    val c = StringBuilder.newBuilder
    c.append(index.toTexSum)
    return c.toString
  }
}

trait Declaration extends AMPL

case class SetDeclaration(name: String, index: Index) extends Declaration {
  override def toAMPL: String = {
    val c = StringBuilder.newBuilder
    c.append("set ")
    c.append(name)
    c.append(";\n")
    return c.toString()
  }
}

case class ParamDeclaration(name: String, vecElem: VecElem) extends Declaration {
  override def toAMPL: String = {
    val c = StringBuilder.newBuilder
    c.append("param ")
    c.append(name)
    vecElem.indices.foreach(v => c.append(v.toAMPLSum))
    c.append(";\n")
    return c.toString().replace("}sum {", ", ")
  }
}

case class DecisionVariableDeclaration(name: String, vecElem: VecElem, equation: Equation) extends Declaration with AMPL {
  override def toAMPL: String = {
    val c = StringBuilder.newBuilder
    c.append("var ")
    c.append(name)
    vecElem.indices.foreach(v => c.append(v.toAMPLSum))
    c.append(equation.op.toAMPL)
    c.append(equation.right.toAMPL)
    c.append(";\n")
    return c.toString().replace("}sum {", ", ")
  }
}

trait ProblemScale {
  def getNumDecisionVariables: Map[String, DecisionVariable]

  def getNumInputVariables: Map[String, InputVar]

  def getNumConstraints: Map[String, Constraint]
}

trait IntermediateRepresentation

trait Constraint extends AMPL with Tex

case class QualifiedConstraint(equation: Equation,
                               qualifier: Qualifier) extends Constraint {
  override def toAMPL: String = {
    val c = StringBuilder.newBuilder
    c.append("s.t. ")
    c.append(qualifier.toAMPL)
    c.append(equation.left.toAMPL)
    c.append(equation.op.toAMPL)
    c.append(equation.right.toAMPL)
    c.append(";")
    return c.toString()
  }

  override def toTex: String = {
    val c = StringBuilder.newBuilder
    c.append("\\[")
    c.append(equation.left.toTex)
    c.append(equation.op.toTex)
    c.append(equation.right.toTex)
    c.append("\\quad")
    c.append(qualifier.toTex)
    c.append("\\]")
    return c.toString()
  }
}

case class SimpleConstraint(equation: Equation) extends Constraint {
  override def toAMPL: String = equation.toAMPL

  override def toTex: String = equation.toTex
}

case class FormulaIR(declarations: List[Declaration],
                     objective: Objective,
                     constraints: List[Constraint]) extends IntermediateRepresentation with AMPL with Tex {
  override def toAMPL: String = {
    val c = StringBuilder.newBuilder
    declarations.foreach(v => c.append(v.toAMPL).append("\n"))
    c.append(objective.toAMPL)
    constraints.foreach(v => c.append(v.toAMPL + "\n"))
    return c.toString()
  }

  override def toTex: String = {
    val c = StringBuilder.newBuilder
    c.append(objective.toTex)
    c.append("\n")
    constraints.foreach(v => c.append(v.toTex + "\n"))
    //merge the sums
    return c.toString().replace("}sum {", ", ")
  }
}