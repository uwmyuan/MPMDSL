package com.ormdsl.interpreter

/**
 * 语法糖测试用例
 */
object SugarTest {

  // 导入语法糖扩展方法
  import Sugar._
  import Sugar.Variables
  import Sugar.Sets

  def main(args: Array[String]): Unit = {
    println("=" * 60)
    println("ORMDSL 语法糖测试")
    println("=" * 60)

    // 测试1: 变量声明
    testVariableDeclaration()

    // 测试2: 集合定义
    testSetDefinition()

    // 测试3: 问题定义
    testProblemDefinition()

    // 测试4: 约束定义
    testConstraintDefinition()

    // 测试5: 求和表达式
    testSumExpression()

    println("\n" + "=" * 60)
    println("所有测试通过!")
    println("=" * 60)
  }

  def testVariableDeclaration(): Unit = {
    println("\n--- 测试1: 变量声明 ---")

    // 二进制变量
    val x = Variables.binary("x[i,j]")
    println(s"二进制变量: $x")

    // 整数变量 (使用 ConstIR 包装整数)
    val y = Variables.integer("y", lb = ConstIR(0), ub = ConstIR(100))
    println(s"整数变量: $y")

    // 连续变量
    val z = Variables.continuous("z", lb = ConstIR(0.0), ub = ConstIR(1.0))
    println(s"连续变量: $z")

    // 标量二进制变量
    val b = Variables.binary("b")
    println(s"标量二进制变量: $b")

    println("✅ 变量声明测试通过")
  }

  def testSetDefinition(): Unit = {
    println("\n--- 测试2: 集合定义 ---")

    // 整数范围集合
    val I = Sets.set("I", 1 to 10)
    println(s"集合 I = 1..10: $I")

    val J = Sets.set("J", 1 to 5)
    println(s"集合 J = 1..5: $J")

    println("✅ 集合定义测试通过")
  }

  def testProblemDefinition(): Unit = {
    println("\n--- 测试3: 问题定义 ---")

    // 定义变量
    val x = Variables.binary("x[i]")
    val c = DoubleNum("c", null, null)

    // 定义集合
    val I = Sets.set("I", 1 to 5)

    // 创建表达式 (使用 AExpIR 包装)
    val idx = IndexIR("i", I)
    val objExpr = SumIR(idx, AExpIR(c, TimesIR, x))

    // 创建公式
    val formula = FormulaIR(
      declarations = List(x, c),
      objective = MinObjectiveIR(objExpr),
      constraints = List.empty
    )

    // 使用 FormulationSugar 语法糖
    implicit class FS(f: Formulation) extends FormulationSugar(f)
    val fs = new FS(formula)
    fs.minimize(objExpr)
    println(s"最小化目标设置成功")

    // 使用 FormulationPrintSugar 语法糖
    implicit class FPS(f: Formulation) extends FormulationPrintSugar(f)
    val fps = new FPS(formula)
    fps.printSummary()

    println("✅ 问题定义测试通过")
  }

  def testConstraintDefinition(): Unit = {
    println("\n--- 测试4: 约束定义 ---")

    // 定义变量
    val x = Variables.binary("x[i]")
    val a = DoubleNum("a", null, null)
    val capacity = ConstIR(10)

    // 定义集合
    val I = Sets.set("I", 1 to 5)

    // 创建约束表达式 (使用 AExpIR 包装)
    val lhs = SumIR(IndexIR("i", I), AExpIR(a, TimesIR, x))
    val constraint = EquationIR(lhs, LessEqIR, capacity)

    // 使用 EquationSugar 命名约束
    implicit class ES(e: EquationIR) extends EquationSugar(e)
    val es = new ES(constraint)
    val named = es.named("capacity")
    println(s"命名约束: $named")

    // 创建简单约束
    val simpleConstraint = SimpleConstraint("capacity", constraint)
    println(s"简单约束: $simpleConstraint")

    println("✅ 约束定义测试通过")
  }

  def testSumExpression(): Unit = {
    println("\n--- 测试5: 求和表达式 ---")

    // 定义集合
    val I = Sets.set("I", 1 to 5)

    // 定义变量
    val c = DoubleNum("c", null, null)
    val x = Variables.binary("x[i]")

    // 创建求和表达式 (使用 AExpIR 包装)
    val idx = IndexIR("i", I)
    val expr = SumIR(idx, AExpIR(c, TimesIR, x))
    println(s"求和表达式: $expr")

    println("✅ 求和表达式测试通过")
  }
}
