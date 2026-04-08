package ormcuopt

/**
 * 线性规划 (LP/QP) Python代码生成器
 *
 * 生成调用 NVIDIA cuOpt 真实 GPU 求解器的 Python 求解脚本，
 * 取代之前基于 PDLP 模板仿真的实现。
 *
 * 后端: cuopt.linear_programming.Problem
 * 安装: pip install nvidia-cuopt
 */

import dsl.ormdsl_intepreter._

object LPGenerator extends CUOPTCodeGenerator {

  // ── 公共入口 ──────────────────────────────────────────────────────────────

  /**
   * 从 FormulaIR 生成完整的 Python LP 求解脚本。
   *
   * 生成的脚本:
   *  1. 使用 cuopt_solver.LPSolver (真实 cuOpt 调用)
   *  2. 自动推断目标方向 (minimize / maximize)
   *  3. 提取所有决策变量及其类型/边界
   *  4. 从 FormulaIR 的约束列表生成约束矩阵
   *  5. 输出求解结果到控制台及 JSON 文件
   */
  def generateLPModel(formula: FormulaIR): String = {
    val modelName     = extractModelName(formula)
    val vars          = extractDecisionVariables(formula)
    val nVars         = vars.length
    val nConstraints  = formula.constraints.size
    val sense         = extractSense(formula)
    val senseStr      = if (sense == "max") "maximize" else "minimize"

    s"""#!/usr/bin/env python3
# -*- coding: utf-8 -*-
\"\"\"
ORMDSL-CUOPT  LP Solver  —  generated
Model      : $modelName
Variables  : $nVars
Constraints: $nConstraints
Backend    : NVIDIA cuOpt  (pip install nvidia-cuopt)
\"\"\"

import sys, os, json, time
import numpy as np

# Make sure cuopt_solver.py is importable
sys.path.insert(0, os.path.join(os.path.dirname(__file__), ".."))
from src.cuopt_solver import LPSolver  # NVIDIA cuOpt wrapper


def build_and_solve():
    print("=" * 60)
    print("ORMDSL-CUOPT LP Solver")
    print(f"  Model      : $modelName")
    print(f"  Variables  : $nVars")
    print(f"  Constraints: $nConstraints")
    print(f"  Sense      : $senseStr")
    print("=" * 60)

    solver = LPSolver(sense="$senseStr")

    # ── 1. Decision variables ──────────────────────────────────────
${generateVarBlock(vars)}

    # ── 2. Objective coefficients ─────────────────────────────────
    c = np.array([
${generateCoeffBlock(formula, vars)}
    ], dtype=np.float64)
    solver.set_objective(c)

    # ── 3. Constraints ────────────────────────────────────────────
${generateConstraintBlock(formula, vars)}

    # ── 4. Solve ──────────────────────────────────────────────────
    print("\\nSolving with NVIDIA cuOpt ...")
    t0     = time.perf_counter()
    result = solver.solve(time_limit=3600.0)
    elapsed = time.perf_counter() - t0

    # ── 5. Results ────────────────────────────────────────────────
    print("\\n" + "=" * 60)
    print(f"Status    : {result['status']}")
    print(f"Objective : {result['objective']:.8g}")
    print(f"Solve time: {elapsed:.4f} s")
    print(f"Iterations: {result['iterations']}")
    print("-" * 60)
    print("Solution:")
${generatePrintBlock(vars)}
    print("=" * 60)

    # Write JSON result
    out = {
        "model":      "$modelName",
        "status":     result["status"],
        "objective":  result["objective"],
        "solve_time": elapsed,
        "solution":   result["variables"],
    }
    outfile = "${modelName}_result.json"
    with open(outfile, "w") as f:
        json.dump(out, f, indent=2)
    print(f"\\nResult written to {{outfile}}")

    return result


if __name__ == "__main__":
    build_and_solve()
"""
  }

  // ── IR 提取助手 ───────────────────────────────────────────────────────────

  private def extractModelName(formula: FormulaIR): String =
    sanitizeName(
      formula.declarations
        .headOption
        .map(_.toString)
        .getOrElse("lp_model")
    )

  private def extractDecisionVariables(formula: FormulaIR): Seq[VariableIR] =
    formula.declarations.collect { case v: VariableIR => v }

  private def extractSense(formula: FormulaIR): String =
    formula.objective match {
      case _: MaximizeIR => "max"
      case _             => "min"
    }

  // ── 代码块生成 ────────────────────────────────────────────────────────────

  /** 生成变量声明块 */
  private def generateVarBlock(vars: Seq[VariableIR]): String =
    vars.map { v =>
      val (lb, ub) = boundsOf(v)
      val vtype    = vtypeOf(v)
      s"""    solver.add_variable("${v.name}", lb=$lb, ub=$ub, vtype="$vtype")"""
    }.mkString("\n")

  /** 生成目标函数系数数组 */
  private def generateCoeffBlock(formula: FormulaIR, vars: Seq[VariableIR]): String = {
    // 尝试从 IR 表达式中读取线性系数
    val coeffMap: Map[String, Double] = formula.objective match {
      case MinimizeIR(expr) => extractLinearCoefficients(expr)
      case MaximizeIR(expr) => extractLinearCoefficients(expr)
      case _                => Map.empty
    }
    vars.map { v =>
      val c = coeffMap.getOrElse(v.name, 0.0)
      f"        $c%.8g,  # ${v.name}"
    }.mkString("\n")
  }

  /** 生成约束定义块 */
  private def generateConstraintBlock(formula: FormulaIR, vars: Seq[VariableIR]): String = {
    if (formula.constraints.isEmpty)
      return "    # No constraints"

    val varIndex: Map[String, Int] = vars.zipWithIndex.map { case (v, i) => v.name -> i }.toMap
    val nVars = vars.length

    formula.constraints.zipWithIndex.map { case (c, idx) =>
      val name   = c.name.getOrElse(s"c_$idx")
      val (rowCoeffs, rhs, sense) = extractConstraintRow(c, varIndex, nVars)
      val rowStr  = rowCoeffs.map(x => f"$x%.8g").mkString(", ")
      s"""    # Constraint: $name
    solver.add_constraint(
        np.array([[${rowStr}]]),
        np.array([${rhs}]),
        sense="${sense}",
        names=["$name"]
    )"""
    }.mkString("\n\n")
  }

  /** 生成结果打印块 */
  private def generatePrintBlock(vars: Seq[VariableIR]): String =
    vars.zipWithIndex.map { case (v, i) =>
      s"""    print(f"  ${v.name} = {result['solution'][$i]:.8g}")"""
    }.mkString("\n")

  // ── 表达式分析助手 ────────────────────────────────────────────────────────

  /**
   * 从表达式中提取线性系数 Map(varName -> coeff)。
   * 只处理线性情形，非线性项系数记为 0.0。
   */
  private def extractLinearCoefficients(expr: ExpIR): Map[String, Double] = expr match {
    case AExpIR(left, PlusIR, right) =>
      mergeCoeffs(extractLinearCoefficients(left), extractLinearCoefficients(right))

    case AExpIR(left, MinusIR, right) =>
      val r = extractLinearCoefficients(right).map { case (k, v) => k -> -v }
      mergeCoeffs(extractLinearCoefficients(left), r)

    case AExpIR(ConstIR(c), TimesIR, v: VariableIR) =>
      Map(v.name -> c)

    case AExpIR(v: VariableIR, TimesIR, ConstIR(c)) =>
      Map(v.name -> c)

    case v: VariableIR =>
      Map(v.name -> 1.0)

    case ConstIR(_) =>
      Map.empty  // constant — no variable

    case SumIR(_, inner) =>
      extractLinearCoefficients(inner)  // simplified: aggregate over all

    case _ =>
      Map.empty
  }

  private def mergeCoeffs(
    a: Map[String, Double],
    b: Map[String, Double]
  ): Map[String, Double] = {
    val keys = a.keySet ++ b.keySet
    keys.map(k => k -> (a.getOrElse(k, 0.0) + b.getOrElse(k, 0.0))).toMap
  }

  /**
   * 从约束 IR 中提取一行 (coefficients, rhs, sense)。
   * 返回稠密系数行、右端项、关系符号。
   */
  private def extractConstraintRow(
    constraint: Constraint,
    varIndex:   Map[String, Int],
    nVars:      Int
  ): (Array[Double], Double, String) = {

    val row   = Array.fill(nVars)(0.0)
    var rhs   = 0.0
    var sense = "<="

    constraint match {
      case SimpleConstraint(_, equation) =>
        val lhsCoeffs = extractLinearCoefficients(equation.left)
        val rhsCoeffs = extractLinearCoefficients(equation.right)
        val rhsConst  = constOf(equation.right)

        // LHS variables → positive row entries
        lhsCoeffs.foreach { case (name, c) =>
          varIndex.get(name).foreach(i => row(i) += c)
        }
        // RHS variables → subtract from row (move to LHS)
        rhsCoeffs.foreach { case (name, c) =>
          varIndex.get(name).foreach(i => row(i) -= c)
        }
        rhs = rhsConst

        sense = equation match {
          case EquationIR(_, LessIR | LessEqIR, _)       => "<="
          case EquationIR(_, GreaterIR | GreaterEqIR, _)  => ">="
          case EquationIR(_, EqualIR, _)                  => "=="
          case _                                           => "<="
        }

      case QualifiedConstraint(_, equation, _) =>
        // Treat same as simple for now (qualifier handled at generation level)
        val lhsCoeffs = extractLinearCoefficients(equation.left)
        val rhsConst  = constOf(equation.right)
        lhsCoeffs.foreach { case (name, c) =>
          varIndex.get(name).foreach(i => row(i) += c)
        }
        rhs = rhsConst
        sense = "<="

      case _ => // unsupported constraint type — zero row
    }

    (row, rhs, sense)
  }

  /** 从表达式中提取常数部分 */
  private def constOf(expr: ExpIR): Double = expr match {
    case ConstIR(n)          => n
    case AExpIR(l, PlusIR, r) => constOf(l) + constOf(r)
    case AExpIR(l, MinusIR, r) => constOf(l) - constOf(r)
    case _                    => 0.0
  }

  // ── 变量属性 ──────────────────────────────────────────────────────────────

  private def boundsOf(v: VariableIR): (String, String) = v match {
    case iv: IntegerDecisionVariable =>
      (iv.lowerBound.toString, iv.upperBound.toString)
    case dv: DoubleDecisionVariable =>
      val ub = if (dv.upperBound >= 1e15) "float('inf')" else dv.upperBound.toString
      (dv.lowerBound.toString, ub)
    case _ => ("0.0", "float('inf')")
  }

  private def vtypeOf(v: VariableIR): String = v match {
    case _: IntegerDecisionVariable => "I"
    case _                          => "C"
  }
}
