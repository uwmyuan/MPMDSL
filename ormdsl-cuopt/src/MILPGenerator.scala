package ormcuopt

/**
 * 混合整数线性规划 (MILP) Python代码生成器
 *
 * 生成调用 NVIDIA cuOpt 真实 GPU MILP 求解器的 Python 求解脚本，
 * 取代之前基于虚拟 Branch-and-Bound 模板的仿真实现。
 *
 * 后端: cuopt.linear_programming.Problem  (整数/二进制变量由 cuOpt 自动路由到 MILP solver)
 * 安装: pip install nvidia-cuopt
 */

import dsl.ormdsl_intepreter._

object MILPGenerator extends CUOPTCodeGenerator {

  // ── 公共入口 ──────────────────────────────────────────────────────────────

  /**
   * 从 FormulaIR 生成完整的 Python MILP 求解脚本。
   *
   * 生成的脚本:
   *  1. 使用 cuopt_solver.MILPSolver (真实 cuOpt MILP 调用)
   *  2. 自动识别整数变量/二进制变量类型
   *  3. 设置 MIP 间隙容忍度与时间限制
   *  4. 输出最优整数解到控制台及 JSON 文件
   */
  def generateMILPModel(formula: FormulaIR): String = {
    val modelName    = extractModelName(formula)
    val vars         = extractDecisionVariables(formula)
    val nVars        = vars.length
    val nConstraints = formula.constraints.size
    val sense        = extractSense(formula)
    val senseStr     = if (sense == "max") "maximize" else "minimize"
    val nIntVars     = vars.count(isIntegerType)
    val nBinVars     = vars.count(isBinaryType)

    s"""#!/usr/bin/env python3
# -*- coding: utf-8 -*-
\"\"\"
ORMDSL-CUOPT  MILP Solver  —  generated
Model         : $modelName
Variables     : $nVars  (integer: $nIntVars, binary: $nBinVars)
Constraints   : $nConstraints
Backend       : NVIDIA cuOpt  (pip install nvidia-cuopt)
\"\"\"

import sys, os, json, time
import numpy as np

sys.path.insert(0, os.path.join(os.path.dirname(__file__), ".."))
from src.cuopt_solver import MILPSolver


def build_and_solve(
    time_limit: float = 3600.0,
    mip_gap:    float = 1e-4,
):
    print("=" * 60)
    print("ORMDSL-CUOPT MILP Solver")
    print(f"  Model      : $modelName")
    print(f"  Variables  : $nVars  (int=$nIntVars, bin=$nBinVars)")
    print(f"  Constraints: $nConstraints")
    print(f"  Sense      : $senseStr")
    print("=" * 60)

    solver = MILPSolver()

    # ── 1. Decision variables ─────────────────────────────────────
${generateVarBlock(vars)}

    # ── 2. Objective coefficients ─────────────────────────────────
    c = np.array([
${generateCoeffBlock(formula, vars)}
    ], dtype=np.float64)
    solver.set_objective(c, sense="$senseStr")

    # ── 3. Constraints ────────────────────────────────────────────
${generateConstraintBlock(formula, vars)}

    # ── 4. Solve ──────────────────────────────────────────────────
    print("\\nSolving with NVIDIA cuOpt MILP ...")
    t0      = time.perf_counter()
    result  = solver.solve(time_limit=time_limit, mip_gap=mip_gap)
    elapsed = time.perf_counter() - t0

    # ── 5. Results ────────────────────────────────────────────────
    print("\\n" + "=" * 60)
    print(f"Status      : {result['status']}")
    print(f"Objective   : {result['objective']:.8g}")
    print(f"Solve time  : {elapsed:.4f} s")
    print(f"B&B nodes   : {result['nodes']}")
    print(f"MIP gap     : {result['mip_gap']:.2e}")
    print("-" * 60)
    print("Integer Solution:")
${generatePrintBlock(vars)}
    print("=" * 60)

    # ── 6. Verify integrality ─────────────────────────────────────
    x = result["solution"]
    int_violation = 0.0
    for i, v in enumerate(x):
        vt = _vtypes[i]
        if vt in ("I", "B"):
            frac = abs(v - round(v))
            if frac > 1e-5:
                print(f"WARNING: x[{i}]={v:.6f} not integer (frac={frac:.2e})")
                int_violation = max(int_violation, frac)
    if int_violation < 1e-5:
        print("Integrality check: PASSED")
    else:
        print(f"Integrality check: FAILED  max_frac={int_violation:.2e}")

    # ── 7. Write JSON result ──────────────────────────────────────
    out = {
        "model":           "$modelName",
        "status":          result["status"],
        "objective":       result["objective"],
        "solve_time_s":    elapsed,
        "bnb_nodes":       result["nodes"],
        "mip_gap":         result["mip_gap"],
        "int_violation":   int_violation,
        "solution":        result["variables"],
    }
    outfile = "${modelName}_milp_result.json"
    with open(outfile, "w") as f:
        json.dump(out, f, indent=2)
    print(f"\\nResult written to {{outfile}}")

    return result


# Variable type list (used for integrality check above)
_vtypes = [${vars.map(v => s""""${vtypeOf(v)}"""").mkString(", ")}]


if __name__ == "__main__":
    import argparse
    p = argparse.ArgumentParser()
    p.add_argument("--time-limit", type=float, default=3600.0)
    p.add_argument("--mip-gap",    type=float, default=1e-4)
    args = p.parse_args()
    build_and_solve(time_limit=args.time_limit, mip_gap=args.mip_gap)
"""
  }

  // ── IR 提取助手 ──────────────────────────────────────────────────────────

  private def extractModelName(formula: FormulaIR): String =
    sanitizeName(
      formula.declarations.headOption.map(_.toString).getOrElse("milp_model")
    )

  private def extractDecisionVariables(formula: FormulaIR): Seq[VariableIR] =
    formula.declarations.collect { case v: VariableIR => v }

  private def extractSense(formula: FormulaIR): String =
    formula.objective match {
      case _: MaximizeIR => "max"
      case _             => "min"
    }

  private def isIntegerType(v: VariableIR): Boolean = v match {
    case _: IntegerDecisionVariable => true
    case _                          => false
  }

  private def isBinaryType(v: VariableIR): Boolean = v match {
    case iv: IntegerDecisionVariable =>
      iv.lowerBound == 0 && iv.upperBound == 1
    case _ => false
  }

  // ── 代码块生成 ────────────────────────────────────────────────────────────

  private def generateVarBlock(vars: Seq[VariableIR]): String =
    vars.map { v =>
      val (lb, ub) = boundsOf(v)
      val vtype    = vtypeOf(v)
      s"""    solver.add_variable("${v.name}", lb=$lb, ub=$ub, vtype="$vtype")"""
    }.mkString("\n")

  private def generateCoeffBlock(formula: FormulaIR, vars: Seq[VariableIR]): String = {
    val coeffMap: Map[String, Double] = formula.objective match {
      case MinimizeIR(expr) => extractLinearCoefficients(expr)
      case MaximizeIR(expr) => extractLinearCoefficients(expr)
      case _                => Map.empty
    }
    vars.map { v =>
      val c = coeffMap.getOrElse(v.name, 0.0)
      f"        $c%.8g,  # ${v.name}  [${vtypeOf(v)}]"
    }.mkString("\n")
  }

  private def generateConstraintBlock(formula: FormulaIR, vars: Seq[VariableIR]): String = {
    if (formula.constraints.isEmpty) return "    # No constraints"

    val varIndex: Map[String, Int] =
      vars.zipWithIndex.map { case (v, i) => v.name -> i }.toMap
    val nVars = vars.length

    formula.constraints.zipWithIndex.map { case (c, idx) =>
      val name = c.name.getOrElse(s"c_$idx")
      val (rowCoeffs, rhs, sense) = extractConstraintRow(c, varIndex, nVars)
      val rowStr = rowCoeffs.map(x => f"$x%.8g").mkString(", ")
      s"""    # Constraint: $name
    solver.add_constraint(
        np.array([[${rowStr}]]),
        np.array([${rhs}]),
        sense="${sense}",
        names=["$name"]
    )"""
    }.mkString("\n\n")
  }

  private def generatePrintBlock(vars: Seq[VariableIR]): String =
    vars.zipWithIndex.map { case (v, i) =>
      val fmt = if (isIntegerType(v)) ":.0f" else ":.8g"
      s"""    print(f"  ${v.name} = {result['solution'][$i]$fmt}  [${vtypeOf(v)}]")"""
    }.mkString("\n")

  // ── 表达式分析 ────────────────────────────────────────────────────────────

  private def extractLinearCoefficients(expr: ExpIR): Map[String, Double] = expr match {
    case AExpIR(l, PlusIR,  r)  =>
      mergeCoeffs(extractLinearCoefficients(l), extractLinearCoefficients(r))
    case AExpIR(l, MinusIR, r)  =>
      val neg = extractLinearCoefficients(r).map { case (k, v) => k -> -v }
      mergeCoeffs(extractLinearCoefficients(l), neg)
    case AExpIR(ConstIR(c), TimesIR, v: VariableIR) => Map(v.name -> c)
    case AExpIR(v: VariableIR, TimesIR, ConstIR(c)) => Map(v.name -> c)
    case v: VariableIR                               => Map(v.name -> 1.0)
    case ConstIR(_)                                  => Map.empty
    case SumIR(_, inner)                             => extractLinearCoefficients(inner)
    case _                                           => Map.empty
  }

  private def mergeCoeffs(a: Map[String, Double], b: Map[String, Double]): Map[String, Double] = {
    (a.keySet ++ b.keySet).map(k => k -> (a.getOrElse(k, 0.0) + b.getOrElse(k, 0.0))).toMap
  }

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
        extractLinearCoefficients(equation.left).foreach { case (name, c) =>
          varIndex.get(name).foreach(i => row(i) += c)
        }
        extractLinearCoefficients(equation.right).foreach { case (name, c) =>
          varIndex.get(name).foreach(i => row(i) -= c)
        }
        rhs   = constOf(equation.right) - constOf(equation.left)
        sense = eqSense(equation)

      case QualifiedConstraint(_, equation, _) =>
        extractLinearCoefficients(equation.left).foreach { case (name, c) =>
          varIndex.get(name).foreach(i => row(i) += c)
        }
        rhs   = constOf(equation.right)
        sense = eqSense(equation)

      case _ =>
    }
    (row, rhs, sense)
  }

  private def eqSense(eq: EquationIR): String = eq match {
    case EquationIR(_, LessIR | LessEqIR, _)      => "<="
    case EquationIR(_, GreaterIR | GreaterEqIR, _) => ">="
    case EquationIR(_, EqualIR, _)                 => "=="
    case _                                         => "<="
  }

  private def constOf(expr: ExpIR): Double = expr match {
    case ConstIR(n)            => n
    case AExpIR(l, PlusIR, r)  => constOf(l) + constOf(r)
    case AExpIR(l, MinusIR, r) => constOf(l) - constOf(r)
    case _                     => 0.0
  }

  // ── 变量属性 ──────────────────────────────────────────────────────────────

  private def boundsOf(v: VariableIR): (String, String) = v match {
    case iv: IntegerDecisionVariable =>
      // Detect binary (0-1 integer)
      if (iv.lowerBound == 0 && iv.upperBound == 1)
        ("0", "1")
      else
        (iv.lowerBound.toString, iv.upperBound.toString)
    case dv: DoubleDecisionVariable =>
      val ub = if (dv.upperBound >= 1e15) "float('inf')" else dv.upperBound.toString
      (dv.lowerBound.toString, ub)
    case _ => ("0", "float('inf')")
  }

  private def vtypeOf(v: VariableIR): String = v match {
    case iv: IntegerDecisionVariable =>
      if (iv.lowerBound == 0 && iv.upperBound == 1) "B" else "I"
    case _ => "C"
  }
}
