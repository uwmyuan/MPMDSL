object Problem {
  def getDvs(exp: ExpIR): Map[String, DecisionVariable] = exp match {
    case ConstIR(n) => Map()
    case VectorElementIR(v, indices) => getDvs(v)
    case SumIR(idx, e) => getDvs(e)
    case AExpIR(e1, op, e2) => getDvs(e1) ++ getDvs(e2)
    case PowExpIR(e, n) => getDvs(e)
    case _ =>Map()
  }

  def getIvs(exp: ExpIR): Map[String, InputIR] = exp match {
    case ConstIR(n) => Map()
    case VectorElementIR(v, indices) => getIvs(v)
    case SumIR(idx, e) => getIvs(e)
    case AExpIR(e1, op, e2) => getIvs(e1) ++ getIvs(e2)
    case PowExpIR(e, n) => getIvs(e)
    case _ =>Map()
  }

  def getOrder(exp: ExpIR): Double = exp match {
    case ConstIR(n) => 0
    case VectorElementIR(v, indices) => getOrder(v)
    case SumIR(idx, e) => getOrder(e)
    case AExpIR(e1, op, e2) => op match {
      case PlusIR => if (getOrder(e1) > getOrder(e2)) getOrder(e1) else getOrder(e2)
      case MinusIR => if (getOrder(e1) > getOrder(e2)) getOrder(e1) else getOrder(e2)
      case TimesIR => getOrder(e1) + getOrder(e2)
      case DivIR => getOrder(e1) + 1 / getOrder(e2)
    }
    case PowExpIR(e, n) => getOrder(e) * n
    case _ => 0
  }

  def getDvs(inputVariable: InputIR): Map[String, DecisionVariable] = inputVariable match {
    case DoubleNum(name, upperbound, lowerbound) => Map()
    case IntegerNum(name, upperbound, lowerbound) => Map()
    case _ => Map()
  }

  def getIvs(inputVariable: InputIR): Map[String, InputIR] = inputVariable match {
    case DoubleNum(name, upperbound, lowerbound) => Map(name -> inputVariable)
    case IntegerNum(name, upperbound, lowerbound) => Map(name -> inputVariable)
    case _ => Map()
  }

  def getOrder(inputVariable: InputIR): Double = inputVariable match {
    case DoubleNum(name, upperbound, lowerbound) => 0
    case IntegerNum(name, upperbound, lowerbound) => 0
    case _ => 0
  }

  def getDvs(decisionVariable: DecisionVariable): Map[String, DecisionVariable] = decisionVariable match {
    case IntegerDecisionVariable(name, lowerbound, upperbound) => Map(name -> decisionVariable)
    case DoubleDecisionVariable(name, lowerbound, upperbound) =>Map(name -> decisionVariable)
    case _ =>Map()
  }

  def getIvs(decisionVariable: DecisionVariable): Map[String, InputVariable] = decisionVariable match {
    case IntegerDecisionVariable(name, lowerbound, upperbound) => Map()
    case DoubleDecisionVariable(name, lowerbound, upperbound) => Map()
    case _ => Map()
  }

  def getOrder(decisionVariable: DecisionVariable): Double = decisionVariable match {
    case IntegerDecisionVariable(name, lowerbound, upperbound) => 1
    case DoubleDecisionVariable(name, lowerbound, upperbound) => 1
    case _ => 1
  }

  def getDvs(objectiveIR: ObjectiveIR): Map[String, DecisionVariable] = objectiveIR match {
    case MinObjectiveIR(e) => getDvs(e)
    case MaxObjectiveIR(e) => getDvs(e)
    case _ => Map()
  }

  def getIvs(objectiveIR: ObjectiveIR): Map[String, InputIR] = objectiveIR match {
    case MinObjectiveIR(e) => getIvs(e)
    case MaxObjectiveIR(e) => getIvs(e)
    case _ => Map()
  }

  def getOrder(objectiveIR: ObjectiveIR): Double = objectiveIR match {
    case MinObjectiveIR(e) => getOrder(e)
    case MaxObjectiveIR(e) => getOrder(e)
    case _ => 0
  }

  def getDvs(equationIR: EquationIR): Map[String, DecisionVariable] = getDvs(equationIR.left) ++ getDvs(equationIR.right)

  def getIvs(equationIR: EquationIR): Map[String, InputIR] = getIvs(equationIR.left) ++ getIvs(equationIR.right)

  def getOrder(equationIR: EquationIR): Double = math.max(getOrder(equationIR.left), getOrder(equationIR.right))

  def getDvs(constraint: Constraint): Map[String, DecisionVariable] = constraint match {
    case QualifiedConstraint(equation, qualifier) => getDvs(equation)
    case SimpleConstraint(equation) => getDvs(equation)
    case DecisionVariableConstraint(equation, decisionVariable) => getDvs(equation)
    case _ => Map()
  }

  def getIvs(constraint: Constraint): Map[String, InputIR] = constraint match {
    case QualifiedConstraint(equation, qualifier) => getIvs(equation)
    case SimpleConstraint(equation) => getIvs(equation)
    case DecisionVariableConstraint(equation, decisionVariable) => getIvs(equation)
    case _ => Map()
  }

  def getOrder(constraint: Constraint): Double = constraint match {
    case QualifiedConstraint(equation, qualifier) => getOrder(equation)
    case SimpleConstraint(equation) => getOrder(equation)
    case DecisionVariableConstraint(equation, decisionVariable) => getOrder(equation)
    case _ => 0
  }

  def getDvs(declaration: Declaration): Map[String, DecisionVariable] = declaration match {
    case IntegerDecisionVariable(name, lowerbound, upperbound) => Map(name -> declaration.asInstanceOf[DecisionVariable])
    case DoubleDecisionVariable(name, lowerbound, upperbound) => Map(name -> declaration.asInstanceOf[DecisionVariable])
    case DoubleNum(name, upperbound, lowerbound) => Map()
    case IntegerNum(name, upperbound, lowerbound) => Map()
  }

  def getIvs(declaration: Declaration): Map[String, InputIR] = declaration match {
    case IntegerDecisionVariable(name, lowerbound, upperbound) => Map()
    case DoubleDecisionVariable(name, lowerbound, upperbound) => Map()
    case DoubleNum(name, upperbound, lowerbound) => Map(name -> declaration.asInstanceOf[InputIR])
    case IntegerNum(name, upperbound, lowerbound) => Map(name -> declaration.asInstanceOf[InputIR])
  }

  def getOrder(declaration: Declaration): Double = declaration match {
    case IntegerDecisionVariable(name, lowerbound, upperbound) => 1
    case DoubleDecisionVariable(name, lowerbound, upperbound) => 1
    case DoubleNum(name, upperbound, lowerbound) => 0
    case IntegerNum(name, upperbound, lowerbound) => 0
  }

  def getDvs(formulaIR: FormulaIR): Map[String, DecisionVariable] = formulaIR.constraints.fold(Map())((v1, v2) => v1.asInstanceOf[Map[String, DecisionVariable]] ++ getDvs(v2.asInstanceOf[Constraint])).asInstanceOf[Map[String, DecisionVariable]] ++ formulaIR.declarations.fold(Map())((v1, v2) => v1.asInstanceOf[Map[String, DecisionVariable]] ++ getDvs(v2.asInstanceOf[Declaration])).asInstanceOf[Map[String, DecisionVariable]] ++ getDvs(formulaIR.objective)

  def getIvs(formulaIR: FormulaIR): Map[String, InputIR] = formulaIR.constraints.fold(Map())((v1, v2) => v1.asInstanceOf[Map[String, InputIR]] ++ getDvs(v2.asInstanceOf[Constraint])).asInstanceOf[Map[String, InputIR]] ++ formulaIR.declarations.fold(Map())((v1, v2) => v1.asInstanceOf[Map[String, Declaration]] ++ getDvs(v2.asInstanceOf[Declaration])).asInstanceOf[Map[String, InputIR]] ++ getIvs(formulaIR.objective)

  def getOrder(formulaIR: FormulaIR): Double = math.max(math.max(formulaIR.constraints.fold(0)((v1, v2) => math.max(v1.asInstanceOf[Double], getOrder(v2.asInstanceOf[Constraint]))).asInstanceOf[Double], formulaIR.declarations.fold(0)((v1, v2) => math.max(v1.asInstanceOf[Double], getOrder(v2.asInstanceOf[Declaration]))).asInstanceOf[Double]), getOrder(formulaIR.objective))

  def getConstraints(formula: FormulaIR): List[Constraint] = formula.constraints
}
