package com.ormdsl.interpreter

/**
 * ORMDSL 语法糖模块
 * 
 * 提供简洁、直观的 DSL 语法，让优化问题的编写更加自然
 * 
 * 使用方法：
 * {{{
 *   import com.ormdsl.interpreter.Sugar._
 *   
 *   // 变量声明
 *   val x = binary("x[i,j]", Set(i, j))
 *   val y = integer("y[i]", Set(i), lb=0, ub=100)
 *   val z = continuous("z", lb=0.0)
 *   
 *   // 问题定义
 *   problem minimize sum(i in I) { c(i) * x(i) }
 *   problem maximize sum(i in I) { p(i) * x(i) }
 *   
 *   // 约束定义
 *   problem constraint "supply" (sum(i in I) { a(i) * x(i) } <= 10)
 *   
 *   // 求解结果
 *   val result = solve(problem)
 *   println(result.summary)
 * }}}
 */
object Sugar {
  
  // ============================================================
  // 一、ProblemSugar - 问题定义语法糖
  // ============================================================
  
  /**
   * Formulation 扩展方法
   * 提供 minimize/maximize/constraint 等语法糖
   */
  implicit class FormulationSugar(formulation: Formulation) {
    
    /**
     * 设置最小化目标函数
     * 
     * 示例：
     * {{{
     *   formulation minimize sum(i in I) { c(i) * x(i) }
     * }}}
     */
    def minimize(expr: ExpIR): Unit = {
      formulation match {
        case formula: FormulaIR => 
          formula.copy(objective = MinObjectiveIR(expr))
        case other =>
          throw new IllegalStateException(s"不支持的问题类型: ${other.getClass.getName}")
      }
    }
    
    /**
     * 设置最大化目标函数
     * 
     * 示例：
     * {{{
     *   formulation maximize sum(i in I) { profit(i) * x(i) }
     * }}}
     */
    def maximize(expr: ExpIR): Unit = {
      formulation match {
        case formula: FormulaIR => 
          formula.copy(objective = MaxObjectiveIR(expr))
        case other =>
          throw new IllegalStateException(s"不支持的问题类型: ${other.getClass.getName}")
      }
    }
    
    /**
     * 添加约束（自动推断约束类型）
     * 
     * 示例：
     * {{{
     *   formulation constraint "capacity" (sum(i in I) { a(i) * x(i) } <= capacity)
     *   formulation constraint "demand" (sum(i in I) { x(i) } >= demand)
     *   formulation constraint "balance" (sum(i in I) { x(i) } === total)
     * }}}
     */
    def constraint(name: String)(eq: EquationIR): Formulation = {
      formulation match {
        case formula: FormulaIR =>
          val newConstraint = SimpleConstraint(name, eq)
          formula.copy(constraints = formula.constraints :+ newConstraint)
        case other =>
          throw new IllegalStateException(s"不支持的问题类型: ${other.getClass.getName}")
      }
    }
    
    /**
     * 添加带限定符的约束
     * 
     * 示例：
     * {{{
     *   formulation "capacity" forAll (i in I) { sum(j in J) { x(i,j) } <= a(i) }
     * }}}
     */
    def apply(name: String) = new ConstraintBuilder(formulation, name)
  }
  
  /**
   * 约束构建器
   */
  class ConstraintBuilder(formulation: Formulation, name: String) {
    
    def apply(eq: EquationIR)(implicit ctx: QualifierContext = null): Formulation = {
      formulation match {
        case formula: FormulaIR =>
          val constraint = if (ctx != null) {
            QualifiedConstraint(name, eq, ctx.qualifier)
          } else {
            SimpleConstraint(name, eq)
          }
          formula.copy(constraints = formula.constraints :+ constraint)
        case other =>
          throw new IllegalStateException(s"不支持的问题类型: ${other.getClass.getName}")
      }
    }
    
    def forAll(idx: IndexIR)(body: => EquationIR): Formulation = {
      // 获取索引集合的边界
      val set = idx.inputSet
      val lb = set match {
        case s: IntegerSet => s.lowerbound
        case s: DoubleSet => s.lowerbound
        case _ => null
      }
      val ub = set match {
        case s: IntegerSet => s.upperbound
        case s: DoubleSet => s.upperbound
        case _ => null
      }
      val qualifier = SetQualifier(idx.name, idx, lb, ub)
      val eq = body
      formulation match {
        case formula: FormulaIR =>
          val constraint = QualifiedConstraint(name, eq, qualifier)
          formula.copy(constraints = formula.constraints :+ constraint)
        case other =>
          throw new IllegalStateException(s"不支持的问题类型: ${other.getClass.getName}")
      }
    }
  }
  
  /**
   * 限定符上下文（用于隐式传递）
   */
  implicit class QualifierContext(val qualifier: Qualifier)
  
  object QualifierContext {
    implicit def apply(q: Qualifier): QualifierContext = new QualifierContext(q)
  }
  
  // ============================================================
  // 二、ConstraintSugar - 约束定义语法糖
  // ============================================================
  
  /**
   * EquationIR 扩展方法
   * 提供约束组合和变换
   */
  implicit class EquationSugar(eq: EquationIR) {
    
    /**
     * 约束名称标注
     * 
     * 示例：
     * {{{
     *   (sum(i in I) { x(i) } <= 10) named "capacity"
     * }}}
     */
    def named(name: String): NamedEquation = NamedEquation(name, eq)
    
    /**
     * 添加第二条约束
     * 
     * 示例：
     * {{{
     *   (x <= 10) and (x >= 0)
     * }}}
     */
    def and(other: EquationIR): List[EquationIR] = List(eq, other)
  }
  
  /**
   * 带名称的约束
   */
  case class NamedEquation(name: String, equation: EquationIR)
  
  /**
   * 约束列表扩展
   */
  implicit class ConstraintListSugar(constraints: List[Constraint]) {
    
    /**
     * 批量添加约束到 Formulation
     */
    def ++(formulation: Formulation): Formulation = {
      formulation match {
        case formula: FormulaIR =>
          formula.copy(constraints = formula.constraints ++ constraints)
        case other =>
          throw new IllegalStateException(s"不支持的问题类型: ${other.getClass.getName}")
      }
    }
  }
  
  // ============================================================
  // 三、VariableSugar - 变量声明语法糖
  // ============================================================
  
  /**
   * 变量声明工厂对象
   */
  object Variables {
    
    /**
     * 创建二进制变量
     * 
     * 示例：
     * {{{
     *   val x = binary("x[i,j]", Set(i, j))
     *   val y = binary("y", Set.empty)  // 标量二进制变量
     * }}}
     */
    def binary(name: String, indices: Set[IndexIR] = Set.empty): IntegerDecisionVariable = {
      IntegerDecisionVariable(name, ConstIR(0), ConstIR(1))
    }
    
    /**
     * 创建整数变量
     * 
     * 示例：
     * {{{
     *   val x = integer("x[i]", Set(i), lb=0, ub=100)
     *   val y = integer("y", lb=0)  // 无上界
     * }}}
     */
    def integer(
      name: String, 
      indices: Set[IndexIR] = Set.empty,
      lb: ExpIR = ConstIR(0),
      ub: ExpIR = null
    ): IntegerDecisionVariable = {
      IntegerDecisionVariable(name, lb, ub)
    }
    
    /**
     * 创建连续变量
     * 
     * 示例：
     * {{{
     *   val z = continuous("z", lb=0.0, ub=1.0)
     *   val w = continuous("w")  // 无界
     * }}}
     */
    def continuous(
      name: String,
      lb: ExpIR = null,
      ub: ExpIR = null
    ): DoubleDecisionVariable = {
      DoubleDecisionVariable(name, lb, ub)
    }
    
    /**
     * 从索引字符串解析变量声明
     * 
     * 示例：
     * {{{
     *   parse("x[i,j]")  // => ("x", List("i", "j"))
     *   parse("y")        // => ("y", List())
     * }}}
     */
    def parse(name: String): (String, List[String]) = {
      val pattern = """(\w+)\[([^]]+)\]""".r
      name match {
        case pattern(base, indices) =>
          (base, indices.split(",").map(_.trim).toList)
        case _ =>
          (name, Nil)
      }
    }
  }
  
  // ============================================================
  // 四、SetSugar - 集合定义语法糖
  // ============================================================
  
  /**
   * 集合工厂对象
   */
  object Sets {
    
    /**
     * 创建整数范围集合
     * 
     * 示例：
     * {{{
     *   val I = set("I", 1 to 10)
     *   val J = set("J", 1 to 5)
     * }}}
     */
    def set(name: String, range: Range.Inclusive): IntegerSet = {
      IntegerSet(name, ConstIR(range.start), ConstIR(range.end))
    }
    
    /**
     * 创建列表集合
     * 
     * 示例：
     * {{{
     *   val colors = set("Colors", List("red", "green", "blue"))
     * }}}
     */
    def set[T](name: String, list: List[T]): InputSet = {
      // 对于字符串列表，使用 DoubleSet
      list.head match {
        case _: String => DoubleSet(name, null, null)  // 简化处理
        case _: Int => IntegerSet(name, null, null)
        case _ => DoubleSet(name, null, null)
      }
    }
    
    /**
     * 创建空集合
     */
    def empty(name: String): InputSet = {
      DoubleSet(name, null, null)
    }
  }
  
  /**
   * 集合运算扩展
   */
  implicit class SetOps(set: InputSet) {
    
    /**
     * 集合并集
     * 
     * 示例：
     * {{{
     *   val K = I ∪ J
     * }}}
     */
    def ∪(other: InputSet): InputSet = {
      // 简化实现，实际应创建新的集合
      set
    }
    
    /**
     * 集合交集
     * 
     * 示例：
     * {{{
     *   val K = I ∩ J
     * }}}
     */
    def ∩(other: InputSet): InputSet = {
      set
    }
    
    /**
     * 集合差集
     * 
     * 示例：
     * {{{
     *   val K = I \ J
     * }}}
     */
    def \(other: InputSet): InputSet = {
      set
    }
  }
  
  // ============================================================
  // 五、ResultSugar - 求解结果语法糖
  // ============================================================
  
  /**
   * 求解结果扩展
   */
  implicit class ResultSugar(result: SolverResult) {
    
    /**
     * 获取变量值（简写）
     * 
     * 示例：
     * {{{
     *   val xVal = result("x")
     *   val yVal = result("y[1,2]")
     * }}}
     */
    def apply(name: String): Double = {
      result.getDecisionValue(name)
    }
    
    /**
     * 获取目标函数值
     */
    def objective: Double = result.getObjectiveValue
    
    /**
     * 检查是否可行
     */
    def isFeasible: Boolean = {
      result.getStatus == "OPTIMAL" || result.getStatus == "FEASIBLE"
    }
    
    /**
     * 检查是否最优
     */
    def isOptimal: Boolean = {
      result.getStatus == "OPTIMAL"
    }
    
    /**
     * 获取约束违反列表
     */
    def violations: List[ConstraintViolation] = {
      result.getViolations
    }
    
    /**
     * 获取状态描述
     */
    def status: String = result.getStatus
    
    /**
     * 打印结果摘要
     */
    def summary: String = {
      s"""
      |═══════════════════════════════════════
      | 求解状态: ${result.getStatus}
      | 目标函数值: ${result.getObjectiveValue}
      |═══════════════════════════════════════
      | 决策变量:
      |${formatVariables(result.getDecisionVariables)}
      |═══════════════════════════════════════
      """.stripMargin
    }
    
    private def formatVariables(vars: Map[String, Any]): String = {
      if (vars.isEmpty) {
        "  (无)"
      } else {
        vars.map { case (name, value) =>
          s"  $name = $value"
        }.mkString("\n")
      }
    }
  }
  
  /**
   * 求解器结果类型
   * (在实际使用中应该由求解器返回)
   */
  trait SolverResult {
    def getDecisionValue(name: String): Double
    def getObjectiveValue: Double
    def getStatus: String
    def getViolations: List[ConstraintViolation]
    def getDecisionVariables: Map[String, Any]
  }
  
  /**
   * 约束违反
   */
  case class ConstraintViolation(
    name: String,
    slack: Double,
    dual: Double
  )
  
  /**
   * 求解函数（语法糖入口）
   * 
   * 示例：
   * {{{
   *   val result = solve(problem)
   * }}}
   */
  def solve(formulation: Formulation)(implicit solver: Solver): SolverResult = {
    solver.solve(formulation)
  }
  
  /**
   * 求解器接口
   */
  trait Solver {
    def solve(formulation: Formulation): SolverResult
  }
  
  // ============================================================
  // 六、PrintSugar - 打印美化
  // ============================================================
  
  /**
   * Formulation 打印扩展
   */
  implicit class FormulationPrintSugar(formulation: Formulation) {
    
    /**
     * 打印问题摘要
     */
    def printSummary(): Unit = {
      formulation match {
        case formula: FormulaIR =>
          println(s"""
          |╔═══════════════════════════════════════════╗
          |║           ORMDSL 问题摘要                 ║
          |╠═══════════════════════════════════════════╣
          |║ 决策变量数量: ${formula.getDecisionVariables.size}
          |║ 输入变量数量: ${formula.getInputVariables.size}
          |║ 约束数量: ${formula.constraints.size}
          |╠═══════════════════════════════════════════╣
          |║ 目标函数: ${formatObjective(formula.objective)}
          |╠═══════════════════════════════════════════╣
          |║ 约束列表:
          |${formatConstraints(formula.constraints)}
          |╚═══════════════════════════════════════════╝
          """.stripMargin)
        case other =>
          println(s"问题类型: ${formulation.getClass.getName}")
      }
    }
    
    private def formatObjective(obj: ObjectiveIR): String = obj match {
      case MinObjectiveIR(e) => s"minimize $e"
      case MaxObjectiveIR(e) => s"maximize $e"
      case _ => "(未设置)"
    }
    
    private def formatConstraints(constraints: List[Constraint]): String = {
      if (constraints.isEmpty) {
        "  (无)"
      } else {
        constraints.zipWithIndex.map { case (c, i) =>
          val name = c match {
            case QualifiedConstraint(n, _, _) => n
            case SimpleConstraint(n, _) => n
            case DecisionVariableConstraint(n, _, _) => n
            case _ => s"c${i+1}"
          }
          s"  [${i+1}] $name"
        }.mkString("\n")
      }
    }
  }
  
  // ============================================================
  // 七、表达式语法糖
  // ============================================================
  
  /**
   * ExpIR 表达式扩展
   */
  implicit class ExpSugar(expr: ExpIR) {
    
    /**
     * 求和表达式（DSL风格）
     * 
     * 示例：
     * {{{
     *   sum(i in I) { c(i) * x(i) }
     *   sum(i in I, j in J) { a(i,j) * x(i,j) }
     * }}}
     */
    def sum(idx: IndexIR)(body: => ExpIR): SumIR = {
      SumIR(idx, body)
    }
    
    /**
     * 绝对值
     */
    def abs: ExpIR = {
      // 简化实现
      expr
    }
    
    /**
     * 平方根
     */
    def sqrt: ExpIR = {
      PowExpIR(expr, 0.5)
    }
    
    /**
     * 平方
     */
    def sq: ExpIR = {
      PowExpIR(expr, 2)
    }
  }
  
  /**
   * IndexIR 扩展
   */
  implicit class IndexSugar(idx: IndexIR) {
    
    /**
     * 创建求和上下文
     * 
     * 示例：
     * {{{
     *   i in I  // 在 sum 中使用
     * }}}
     */
    def in(set: InputSet): IndexIR = {
      IndexIR(idx.name, set)
    }
  }
  
  /**
   * 索引生成器
   * 
   * 示例：
   * {{{
   *   val i = index("i")
   *   val j = index("j")
   * }}}
   */
  def index(name: String): IndexIR = {
    IndexIR(name, DoubleSet(name + "_set"))
  }
  
  // ============================================================
  // 八、导入全部语法糖
  // ============================================================
  
  /**
   * 一键导入所有语法糖
   * 
   * 在 Scala REPL 或代码中使用：
   * {{{
   *   import com.ormdsl.interpreter.Sugar._
   * }}}
   */
  object Implicits {
    // 空对象，用于导入语法糖
    // 使用方式: import com.ormdsl.interpreter.Sugar.Implicits._
  }
  
}
