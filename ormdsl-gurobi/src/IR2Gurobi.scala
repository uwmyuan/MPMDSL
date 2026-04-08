package ormgurobi

/**
 * 中间表示(IR)到Gurobi代码生成器
 * 将ORMDSL的IR转换为Gurobi Python API调用
 */

import dsl.ormdsl_intepreter._

object IR2Gurobi {
  
  // 变量类型映射
  private def mapVarType(ir: IntermediateRepresentation): (String, String) = ir match {
    case _: IntegerDecisionVariable => ("0", "GRB.INTEGER")
    case _: DoubleDecisionVariable => ("0.0", "GRB.CONTINUOUS")
    case _ => ("0.0", "GRB.CONTINUOUS")
  }
  
  // 表达式转Gurobi代码
  def generate(exp: ExpIR): String = exp match {
    case ConstIR(n) => 
      if (n % 1 == 0) n.toInt.toString else n.toString
    
    case v: IntegerDecisionVariable => 
      v.name
    
    case v: DoubleDecisionVariable => 
      v.name
    
    case v: VectorElementIR =>
      val varName = v.v match {
        case vec: VectorIR => vec.toString
        case _ => v.v.toString
      }
      val indices = v.indices.map { idx => idx.name }.mkString("[", "][", "]")
      s"$varName$indices"
    
    case SumIR(idx, e) =>
      val setName = idx match {
        case IndexIR(name, inputSet) => inputSet match {
          case DoubleSet(n, _, _) => n
          case IntegerSet(n, _, _) => n
          case _ => "S"
        }
      }
      s"sum(${setName}, {idx.name} => ${generate(e)})"
    
    case AExpIR(e1, op, e2) =>
      val left = generate(e1)
      val right = generate(e2)
      val opStr = op match {
        case PlusIR => "+"
        case MinusIR => "-"
        case TimesIR => "*"
        case DivIR => "/"
      }
      s"($left $opStr $right)"
    
    case PowExpIR(e, n) =>
      s"(${generate(e)} ** $n)"
    
    case _ => "expr"
  }
  
  // 约束转Gurobi代码
  def generateConstraint(constraint: Constraint): List[String] = constraint match {
    case SimpleConstraint(name, equation) =>
      val (lhs, op, rhs) = generateEquation(equation)
      List(s"# Constraint: $name", s"""model.addConstr($lhs $op $rhs, name="$name")""")
    
    case QualifiedConstraint(name, equation, qualifier) =>
      val (lhs, op, rhs) = generateEquation(equation)
      val setInfo = generateQualifier(qualifier)
      List(
        s"# Qualified constraint: $name",
        s"# Set: $setInfo",
        s"""model.addConstr($lhs $op $rhs, name="$name")"""
      )
    
    case DecisionVariableConstraint(name, equation, dv) =>
      val (lhs, op, rhs) = generateEquation(equation)
      List(
        s"# Decision variable constraint: $name",
        s"""model.addConstr($lhs $op $rhs, name="$name")"""
      )
    
    case _ => List()
  }
  
  // 方程转三元组 (lhs, op, rhs)
  private def generateEquation(eq: EquationIR): (String, String, String) = {
    val lhs = generate(eq.left)
    val rhs = generate(eq.right)
    val op = eq match {
      case EquationIR(_, op: CopIR, _) => op match {
        case LessIR => "<"
        case GreaterIR => ">"
        case LessEqIR => "<="
        case GreaterEqIR => ">="
        case EqualIR => "=="
      }
    }
    (lhs, op, rhs)
  }
  
  // 限定符转字符串
  private def generateQualifier(q: Qualifier): String = q match {
    case SetQualifier(name, idx, lower, upper) =>
      val lb = generate(lower)
      val ub = generate(upper)
      s"$name in [$lb..$ub]"
    case CompoundQualifier(name, qualifiers) =>
      qualifiers.map(generateQualifier).mkString(" and ")
  }
  
  // 目标函数转Gurobi代码
  def generateObjective(obj: ObjectiveIR): (String, String) = obj match {
    case MinObjectiveIR(e) =>
      (generate(e), "GRB.MINIMIZE")
    case MaxObjectiveIR(e) =>
      (generate(e), "GRB.MAXIMIZE")
  }
  
  // 完整模型转Gurobi Python代码
  def generateModel(formula: FormulaIR): String = {
    val model = GurobiModel(formula.declarations.headOption.map(_.toString).getOrElse("model"))
    
    // 收集所有决策变量
    val varDecls = formula.declarations.flatMap {
      case v: IntegerDecisionVariable => 
        val lb = Option(v.lowerbound).map(generate).getOrElse("0")
        val ub = Option(v.upperbound).map(generate).getOrElse("GRB.INFINITY")
        Some(s"x_${v.name} = model.addVar(lb=$lb, ub=$ub, vtype=GRB.INTEGER, name='${v.name}')")
      case v: DoubleDecisionVariable =>
        val lb = Option(v.lowerbound).map(generate).getOrElse("0")
        val ub = Option(v.upperbound).map(generate).getOrElse("GRB.INFINITY")
        Some(s"x_${v.name} = model.addVar(lb=$lb, ub=$ub, vtype=GRB.CONTINUOUS, name='${v.name}')")
      case _ => None
    }
    
    varDecls.foreach(code => model.addExpression(code))
    
    // 添加约束
    formula.constraints.foreach { c =>
      generateConstraint(c).foreach(code => model.addExpression(code))
    }
    
    // 设置目标函数
    val (expr, sense) = generateObjective(formula.objective)
    formula.objective match {
      case _: MinObjectiveIR => model.setObjective(expr, Minimize)
      case _: MaxObjectiveIR => model.setObjective(expr, Maximize)
    }
    
    model.generatePythonCode()
  }
  
  // 生成变量声明
  def generateVariables(declarations: List[Declaration]): List[String] = {
    declarations.flatMap {
      case v: IntegerDecisionVariable =>
        val lb = Option(v.lowerbound).map(generate).getOrElse("0")
        val ub = Option(v.upperbound).map(generate).getOrElse("GRB.INFINITY")
        List(
          s"# Integer variable: ${v.name}",
          s"${v.name} = model.addVar(lb=$lb, ub=$ub, vtype=GRB.INTEGER, name='${v.name}')"
        )
      case v: DoubleDecisionVariable =>
        val lb = Option(v.lowerbound).map(generate).getOrElse("0")
        val ub = Option(v.upperbound).map(generate).getOrElse("GRB.INFINITY")
        List(
          s"# Continuous variable: ${v.name}",
          s"${v.name} = model.addVar(lb=$lb, ub=$ub, vtype=GRB.CONTINUOUS, name='${v.name}')"
        )
      case IndexIR(name, inputSet) =>
        List(s"# Index: $name in $inputSet")
      case _ => List()
    }
  }
  
  // 生成集合操作
  def generateSetOperation(op: String, set: String, expr: String): String = op match {
    case "sum" => s"gp.quicksum($expr for $set)"
    case "prod" => s"gp.prod($expr for $set)"
    case "min" => s"min($expr for $set)"
    case "max" => s"max($expr for $set)"
    case _ => s"$op($expr for $set)"
  }
}
