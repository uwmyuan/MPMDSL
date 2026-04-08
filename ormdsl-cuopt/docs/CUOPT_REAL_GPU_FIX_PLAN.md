# ORMDSL-CUOPT 真实GPU求解器修复编码计划

## 1. 概述

### 1.1 目标
使用 **NVIDIA cuOpt** 真实GPU求解器替换当前的仿真代码，实现：
- LP (线性规划) - cuOpt LP求解器
- MILP (混合整数规划) - cuOpt MILP求解器  
- VRP/TSP (路径优化) - cuOpt Routing求解器

### 1.2 参考文档
- NVIDIA cuOpt官方文档: https://docs.nvidia.com/cuopt/user-guide/latest/
- 安装方式: `pip install nvidia-cuopt`

### 1.3 cuOpt Python API 核心组件

```python
from cuopt.linear_programming import problem, solver_settings

# 问题建模
prob = problem.Problem()
x1 = prob.add_variable("x1", 0, problem.INF, 'C')  # 变量名, 下界, 上界, 类型
x2 = prob.add_variable("x2", 0, problem.INF, 'C')

# 目标函数
prob.set_objective(3*x1 + 2*x2, sense=problem.MAXIMIZE)  # 或 MINIMIZE

# 约束
prob.add_constraint(x1 + 2*x2 <= 10, "c1")
prob.add_constraint(2*x1 + x2 <= 8, "c2")

# 求解器设置
settings = solver_settings.SolverSettings()
settings.time_limit = 10.0

# 求解
solution = prob.solve(settings=settings)
print(f"最优值: {solution.objective_value}")
print(f"状态: {solution.status}")  # OPTIMAL, SUBOPTIMAL, INFEASIBLE
```

---

## 2. 文件修复清单

### 2.1 核心修复文件

| 文件 | 当前状态 | 修复后功能 |
|------|----------|-----------|
| `src/CUOPTCodeGenerator.scala` | 基类 | 保留基础架构 |
| `src/IR2CUOPT.scala` | IR转换器 | 扩展支持cuOpt格式 |
| `src/PythonAPI.scala` | 仿真调用 | 真实cuOpt调用 |
| `src/LPGenerator.scala` | PDLP仿真 | cuOpt LP |
| `src/MILPGenerator.scala` | B&B仿真 | cuOpt MILP |
| `src/RoutingGenerator.scala` | ALNS仿真 | cuOpt Routing |
| `tests/benchmark_test.py` | Mock测试 | 真实GPU测试 |

---

## 3. 详细编码计划

### Phase 1: IR到cuOpt转换层 (第1-2天)

#### 3.1 创建 `src/CUOPTAdapter.scala`

```scala
package ormcuopt

import dsl.ormdsl_intertepreter._
import java.{util => ju}
import scala.jdk.CollectionConverters._

/**
 * ORMDSL中间表示(IR)到cuOpt模型的转换器
 */
object CUOPTAdapter {
  
  /**
   * 将FormulaIR转换为cuOpt Problem对象
   */
  def formulaToCuOptProblem(formula: FormulaIR): jpype.JClass = {
    // 获取cuopt.linear_programming.problem模块
    val cuoptProblem = jpype.JClass("cuopt.linear_programming.problem.Problem")
    val prob = cuoptProblem()
    
    // 1. 添加决策变量
    formula.declarations.foreach {
      case v: VariableIR => addVariable(prob, v)
      case _ =>
    }
    
    // 2. 设置目标函数
    val objExpr = formula.objective match {
      case MinimizeIR(expr) => buildExpression(expr)
      case MaximizeIR(expr) => buildExpression(expr)
    }
    
    val sense = formula.objective match {
      case _: MinimizeIR => cuoptProblem.getField("MINIMIZE").get(null)
      case _: MaximizeIR => cuoptProblem.getField("MAXIMIZE").get(null)
    }
    prob.set_objective(objExpr, sense)
    
    // 3. 添加约束
    formula.constraints.foreach { c =>
      val expr = buildExpression(c.expression)
      val sense = c match {
        case _: LessThanOrEqual => "<="
        case _: GreaterThanOrEqual => ">="
        case _: Equal => "=="
      }
      prob.add_constraint(expr, sense, c.name.getOrElse("c"))
    }
    
    prob
  }
  
  /**
   * 添加单个变量到cuOpt问题
   */
  private def addVariable(prob: Any, v: VariableIR): Unit = {
    val (lb, ub, vtype) = v match {
      case iv: IntegerDecisionVariable => 
        (iv.lowerBound.toDouble, iv.upperBound.toDouble, "I")
      case dv: DoubleDecisionVariable =>
        (dv.lowerBound, dv.upperBound, "C")
      case _ => (0.0, Double.PositiveInfinity, "C")
    }
    
    prob.add_variable(v.name, lb, ub, vtype)
  }
  
  /**
   * 从表达式构建cuOpt线性表达式
   */
  private def buildExpression(expr: ExpressionIR): jpype.JObject = {
    // 实现表达式树遍历
    // 返回cuopt.linear_programming.problem.LinearExpression
    ???
  }
}
```

### Phase 2: Python API集成 (第3-4天)

#### 3.2 重写 `src/PythonAPI.scala`

```scala
package ormcuopt

/**
 * Python API包装器 - 集成真实NVIDIA cuOpt
 */
object PythonAPIGenerator {
  
  def generatePythonWrapper(): String = {
    s"""#!/usr/bin/env python3
# -*- coding: utf-8 -*-
\"\"\"
ORMDSL-CUOPT Python API
集成NVIDIA cuOpt GPU加速优化求解器
\"\"\"

import numpy as np
from typing import Dict, Any, Optional, Union

# 导入cuOpt
try:
    from cuopt.linear_programming import problem as cuopt_problem
    from cuopt.linear_programming import solver_settings
    from cuopt.routing import routing
    _CUOPT_AVAILABLE = True
except ImportError:
    _CUOPT_AVAILABLE = False
    print("Warning: cuOpt not installed. Run: pip install nvidia-cuopt")


class LPSolver:
    \"\"\"
    线性规划求解器 - 使用NVIDIA cuOpt LP
    
    Example:
        >>> solver = LPSolver()
        >>> solver.set_objective([1.0, 2.0])  # min c·x
        >>> solver.add_constraint([[1.0, 1.0]], [4.0])  # A·x <= b
        >>> result = solver.solve()
        >>> print(f"Optimal: {result['objective']:.4f}")
    \"\"\"
    
    def __init__(self, sense: str = "minimize"):
        if not _CUOPT_AVAILABLE:
            raise RuntimeError("cuOpt not installed. Run: pip install nvidia-cuopt")
        
        self.prob = cuopt_problem.Problem()
        self.n_vars = 0
        self.var_names = []
        self.sense = sense.lower()
        
    def add_variable(
        self, 
        name: str, 
        lb: float = 0.0, 
        ub: float = float('inf'),
        vtype: str = 'C'
    ) -> int:
        \"\"\"添加变量，返回变量索引\"\"\"
        v = self.prob.add_variable(name, lb, ub, vtype)
        self.var_names.append(name)
        self.n_vars += 1
        return self.n_vars - 1
    
    def set_objective(self, c: np.ndarray, sense: Optional[str] = None):
        \"\"\"设置目标函数系数\"\"\"
        self.c = np.asarray(c, dtype=np.float64)
        
        if sense is None:
            sense = self.sense
        else:
            sense = sense.lower()
        
        obj_sense = (cuopt_problem.MAXIMIZE if sense == "maximize" 
                     else cuopt_problem.MINIMIZE)
        
        # 构建线性表达式
        obj_expr = sum(c * self.prob.get_variable(name) 
                       for c, name in zip(self.c, self.var_names))
        self.prob.set_objective(obj_expr, sense=obj_sense)
    
    def add_constraint(
        self, 
        A: np.ndarray, 
        b: np.ndarray,
        sense: str = "<="
    ):
        \"\"\"添加约束 A·x {sense} b\"\"\"
        A = np.asarray(A, dtype=np.float64)
        b = np.asarray(b, dtype=np.float64)
        
        for i in range(len(b)):
            row = A[i] if len(A.shape) > 1 else A
            expr = sum(val * self.prob.get_variable(name) 
                       for val, name in zip(row, self.var_names))
            
            constraint_sense = {
                "<=": cuopt_problem.LESS_EQUALS,
                ">=": cuopt_problem.GREATER_EQUALS,
                "==": cuopt_problem.EQUALS
            }.get(sense, cuopt_problem.LESS_EQUALS)
            
            self.prob.add_constraint(expr, constraint_sense, f"c_{i}")
    
    def solve(self, time_limit: float = 3600.0) -> Dict[str, Any]:
        \"\"\"求解LP\"\"\"
        settings = solver_settings.SolverSettings()
        settings.time_limit = time_limit
        
        solution = self.prob.solve(settings=settings)
        
        # 提取结果
        status_map = {
            cuopt_problem.OPTIMAL: "optimal",
            cuopt_problem.SUBOPTIMAL: "suboptimal", 
            cuopt_problem.INFEASIBLE: "infeasible",
            cuopt_problem.UNBOUNDED: "unbounded"
        }
        
        result = {
            "status": status_map.get(solution.status, "unknown"),
            "objective": solution.objective_value,
            "solution": np.array([
                solution.get_value(self.prob.get_variable(name))
                for name in self.var_names
            ]),
            "iterations": solution.iterations if hasattr(solution, 'iterations') else 0
        }
        
        return result


class MILPSolver:
    \"\"\"
    混合整数规划求解器 - 使用NVIDIA cuOpt MILP
    \"\"\"
    
    def __init__(self):
        if not _CUOPT_AVAILABLE:
            raise RuntimeError("cuOpt not installed. Run: pip install nvidia-cuopt")
        
        self.prob = cuopt_problem.Problem()
        self.n_vars = 0
        self.var_names = []
        self.vtypes = []
    
    def add_variable(
        self, 
        name: str, 
        lb: float = 0.0, 
        ub: float = float('inf'),
        vtype: str = 'C'
    ) -> int:
        \"\"\"添加变量: 'C'=连续, 'I'=整数, 'B'=二进制\"\"\"
        v = self.prob.add_variable(name, lb, ub, vtype)
        self.var_names.append(name)
        self.vtypes.append(vtype)
        self.n_vars += 1
        return self.n_vars - 1
    
    def set_objective(self, c: np.ndarray, sense: str = "minimize"):
        \"\"\"设置目标函数\"\"\"
        self.c = np.asarray(c, dtype=np.float64)
        sense = sense.lower()
        obj_sense = (cuopt_problem.MAXIMIZE if sense == "maximize" 
                     else cuopt_problem.MINIMIZE)
        
        obj_expr = sum(c * self.prob.get_variable(name) 
                       for c, name in zip(self.c, self.var_names))
        self.prob.set_objective(obj_expr, sense=obj_sense)
    
    def add_constraint(self, A: np.ndarray, b: np.ndarray, sense: str = "<="):
        \"\"\"添加约束\"\"\"
        A = np.asarray(A, dtype=np.float64)
        b = np.asarray(b, dtype=np.float64)
        
        for i in range(len(b)):
            row = A[i] if len(A.shape) > 1 else A
            expr = sum(val * self.prob.get_variable(name) 
                       for val, name in zip(row, self.var_names))
            
            constraint_sense = {
                "<=": cuopt_problem.LESS_EQUALS,
                ">=": cuopt_problem.GREATER_EQUALS,
                "==": cuopt_problem.EQUALS
            }.get(sense, cuopt_problem.LESS_EQUALS)
            
            self.prob.add_constraint(expr, constraint_sense, f"c_{i}")
    
    def solve(self, time_limit: float = 3600.0, gap: float = 0.01) -> Dict[str, Any]:
        \"\"\"求解MILP\"\"\"
        settings = solver_settings.SolverSettings()
        settings.time_limit = time_limit
        settings.mip_gap = gap  # 设置MIP间隙容忍度
        
        solution = self.prob.solve(settings=settings)
        
        status_map = {
            cuopt_problem.OPTIMAL: "optimal",
            cuopt_problem.SUBOPTIMAL: "gap_exceeded",
            cuopt_problem.INFEASIBLE: "infeasible",
            cuopt_problem.UNBOUNDED: "unbounded"
        }
        
        result = {
            "status": status_map.get(solution.status, "unknown"),
            "objective": solution.objective_value,
            "solution": np.array([
                solution.get_value(self.prob.get_variable(name))
                for name in self.var_names
            ]),
            "nodes": solution.num_nodes if hasattr(solution, 'num_nodes') else 0,
            "mip_gap": solution.mip_gap if hasattr(solution, 'mip_gap') else gap
        }
        
        return result


class VRPSolver:
    \"\"\"
    车辆路径求解器 - 使用NVIDIA cuOpt Routing
    
    Example:
        >>> solver = VRPSolver(n_locations=6, n_vehicles=2)
        >>> solver.set_cost_matrix(cost_matrix)
        >>> solver.set_demands(demands)
        >>> solver.set_capacities([15, 15])
        >>> result = solver.solve()
        >>> print(f"Total cost: {result['total_cost']}")
        >>> for route in result['routes']:
        ...     print(f"Route: {' -> '.join(map(str, route))}")
    \"\"\"
    
    def __init__(self, n_locations: int, n_vehicles: int):
        if not _CUOPT_AVAILABLE:
            raise RuntimeError("cuOpt not installed. Run: pip install nvidia-cuopt")
        
        self.n_locations = n_locations
        self.n_vehicles = n_vehicles
        
        # cuOpt Routing API
        self.routing_data = routing.RoutingData()
        self.solution = None
    
    def set_cost_matrix(self, costs: np.ndarray):
        \"\"\"设置成本矩阵 (n+1) x (n+1), node 0 是仓库\"\"\"
        self.cost_matrix = np.asarray(costs, dtype=np.float64)
        self.routing_data.set_cost_matrix(self.cost_matrix)
    
    def set_demands(self, demands: np.ndarray):
        \"\"\"设置客户需求 (n,)\"\"\"
        self.demands = np.asarray(demands, dtype=np.float64)
        self.routing_data.set_demands(self.demands)
    
    def set_capacities(self, capacities: np.ndarray):
        \"\"\"设置车辆容量\"\"\"
        self.capacities = np.asarray(capacities, dtype=np.float64)
        self.routing_data.set_vehicle_capacities(self.capacities)
    
    def set_time_windows(self, windows: np.ndarray):
        \"\"\"设置时间窗 [start, end] for each location\"\"\"
        self.time_windows = np.asarray(windows, dtype=np.float64)
        self.routing_data.set_time_windows(self.time_windows)
    
    def solve(
        self, 
        max_runtime: float = 30.0,
        vehicle_count: Optional[int] = None
    ) -> Dict[str, Any]:
        \"\"\"求解VRP\"\"\"
        if vehicle_count is None:
            vehicle_count = self.n_vehicles
        
        # 配置求解器
        solver_config = routing.SolverConfig()
        solver_config.max_runtime = max_runtime
        
        # 求解
        self.solution = self.routing_data.solve(solver_config)
        
        # 提取结果
        routes = []
        route_costs = []
        
        for v in range(vehicle_count):
            route = self.solution[f"route_{v}"]
            if route:
                routes.append(list(route))
                route_costs.append(self.solution[f"cost_{v}"])
        
        return {
            "total_cost": self.solution.total_cost,
            "num_routes": len(routes),
            "routes": routes,
            "route_costs": route_costs,
            "status": self.solution.status
        }


# 便捷函数
def solve_lp(
    c: np.ndarray,
    A: np.ndarray,
    b: np.ndarray,
    sense: str = "minimize",
    **kwargs
) -> Dict[str, Any]:
    \"\"\"求解LP的便捷函数\"\"\"
    solver = LPSolver(sense)
    for i, coef in enumerate(c):
        solver.add_variable(f"x_{i}")
    solver.set_objective(c, sense)
    solver.add_constraint(A, b)
    return solver.solve(**kwargs)


def solve_milp(
    c: np.ndarray,
    A: np.ndarray,
    b: np.ndarray,
    vtypes: str = "C",
    sense: str = "minimize",
    **kwargs
) -> Dict[str, Any]:
    \"\"\"求解MILP的便捷函数\"\"\"
    solver = MILPSolver()
    for i, (coef, vtype) in enumerate(zip(c, vtypes)):
        solver.add_variable(f"x_{i}", vtype=vtype)
    solver.set_objective(c, sense)
    solver.add_constraint(A, b)
    return solver.solve(**kwargs)
"""
  }
  
  def generateQuickStart(): String = {
    """# Quick Start - ORMDSL-CUOPT with NVIDIA cuOpt

## 安装

```bash
pip install nvidia-cuopt
```

## 1. 线性规划 (LP)

```python
from cuopt_solver import LPSolver
import numpy as np

# 创建求解器
solver = LPSolver(sense="minimize")

# 添加变量: x, y >= 0
solver.add_variable("x", lb=0, ub=float('inf'))
solver.add_variable("y", lb=0, ub=float('inf'))

# 设置目标: min 1*x + 2*y
solver.set_objective(np.array([1.0, 2.0]))

# 添加约束:
#   x + y <= 4
#   2*x + y <= 5
A = np.array([[1.0, 1.0], [2.0, 1.0]])
b = np.array([4.0, 5.0])
solver.add_constraint(A, b)

# 求解
result = solver.solve(time_limit=10.0)
print(f"Status: {result['status']}")
print(f"Objective: {result['objective']:.4f}")
print(f"Solution: x={result['solution'][0]:.4f}, y={result['solution'][1]:.4f}")
```

## 2. 混合整数规划 (MILP)

```python
from cuopt_solver import MILPSolver
import numpy as np

solver = MILPSolver()

# 添加变量: x, y 整数
solver.add_variable("x", vtype='I')  # 整数
solver.add_variable("y", vtype='I')   # 整数

# min 3*x + 2*y
solver.set_objective(np.array([3.0, 2.0]))

# 约束
solver.add_constraint(np.array([[1.0, 1.0]]), np.array([10.0]))

result = solver.solve(time_limit=30.0, gap=0.001)
print(f"Optimal: {result['objective']}")
```

## 3. 车辆路径问题 (VRP)

```python
from cuopt_solver import VRPSolver
import numpy as np

# 5个客户 + 1个仓库
n_customers = 5
n_vehicles = 2

solver = VRPSolver(n_locations=n_customers+1, n_vehicles=n_vehicles)

# 成本矩阵 (6x6, node 0 是仓库)
costs = np.array([
    [0, 10, 15, 20, 12, 8],
    [10, 0, 18, 25, 14, 9],
    [15, 18, 0, 22, 16, 11],
    [20, 25, 22, 0, 19, 15],
    [12, 14, 16, 19, 0, 13],
    [8, 9, 11, 15, 13, 0]
])
solver.set_cost_matrix(costs)

# 需求 (不包括仓库)
solver.set_demands(np.array([0, 5, 3, 6, 4, 2]))

# 容量
solver.set_capacities(np.array([15, 15]))

# 求解
result = solver.solve(max_runtime=30.0)
print(f"Total distance: {result['total_cost']:.2f}")
```
"""
  }
}
```

### Phase 3: 代码生成器修复 (第5-7天)

#### 3.3 重写 `src/LPGenerator.scala`

```scala
package ormcuopt

/**
 * 线性规划GPU代码生成器
 * 使用NVIDIA cuOpt作为底层求解器
 */
object LPGenerator extends CUOPTCodeGenerator {
  
  // 从FormulaIR生成Python求解脚本
  def generateLPModel(formula: FormulaIR): String = {
    val modelName = sanitizeName(
      formula.declarations.headOption.map(_.toString).getOrElse("lp_model")
    )
    val nVars = countVariables(formula)
    val nConstraints = formula.constraints.size
    
    s"""
#!/usr/bin/env python3
# -*- coding: utf-8 -*-
\"\"\"
ORMDSL-CUOPT LP Solver
Generated from: $modelName
Backend: NVIDIA cuOpt
\"\"\"

import numpy as np
import sys
sys.path.insert(0, '${"$"}{sys.path[0]}/../src')

from cuopt_solver import LPSolver, solve_lp

def main():
    print("=" * 60)
    print(f"ORMDSL-CUOPT LP Solver")
    print(f"Model: $modelName")
    print(f"Variables: $nVars")
    print(f"Constraints: $nConstraints")
    print("=" * 60)
    
    # 创建求解器
    solver = LPSolver(sense="${formula.objective match {
      case _: MinimizeIR => "minimize"
      case _: MaximizeIR => "maximize"
    }}")
    
    # 添加变量
${generateVariableDefinitions(formula)}
    
    # 设置目标函数
    c = np.array([
${generateObjectiveCoefficients(formula)}
    ], dtype=np.float64)
    solver.set_objective(c)
    
    # 添加约束
${generateConstraintDefinitions(formula)}
    
    # 求解
    print("\\nSolving...")
    result = solver.solve(time_limit=3600.0)
    
    # 输出结果
    print("\\n" + "=" * 60)
    print(f"Status: {result['status']}")
    print(f"Objective Value: {result['objective']:.6f}")
    print("-" * 60)
    print("Solution:")
${generateSolutionOutput(formula)}
    print("=" * 60)
    
    return result

if __name__ == "__main__":
    main()
"""
  }
  
  private def sanitizeName(name: String): String = {
    name.replaceAll("[^a-zA-Z0-9_]", "_")
  }
  
  private def countVariables(formula: FormulaIR): Int = {
    formula.declarations.count {
      case _: VariableIR => true
      case _ => false
    }
  }
  
  private def generateVariableDefinitions(formula: FormulaIR): String = {
    formula.declarations.collect {
      case v: VariableIR =>
        val (lb, ub) = v match {
          case iv: IntegerDecisionVariable => 
            (iv.lowerBound.toString, iv.upperBound.toString)
          case dv: DoubleDecisionVariable =>
            (dv.lowerBound.toString, dv.upperBound.toString)
          case _ => ("0.0", "float('inf')")
        }
        s"""solver.add_variable("${v.name}", lb=$lb, ub=$ub)"""
    }.mkString("\n    ")
  }
  
  private def generateObjectiveCoefficients(formula: FormulaIR): String = {
    formula.objective match {
      case MinimizeIR(LinearExpressionIR(coeffs, _, _)) =>
        coeffs.map(c => s"        ${c.toString},").mkString("\n")
      case MaximizeIR(LinearExpressionIR(coeffs, _, _)) =>
        coeffs.map(c => s"        ${c.toString},").mkString("\n")
      case _ => "        0.0,"
    }
  }
  
  private def generateConstraintDefinitions(formula: FormulaIR): String = {
    formula.constraints.map { c =>
      s"""# ${c.name.getOrElse("unnamed")}
    solver.add_constraint(
        np.array([${generateCoefficients(c.expression)}]),
        np.array([${generateRHS(c.expression)}])
    )"""
    }.mkString("\n")
  }
  
  private def generateSolutionOutput(formula: FormulaIR): String = {
    formula.declarations.collect {
      case v: VariableIR =>
        s"""print(f"  ${v.name} = {result['solution'][${
          formula.declarations.indexWhere(_.asInstanceOf[VariableIR].name == v.name)
        }]:.6f}")"""
    }.mkString("\n")
  }
}
```

#### 3.4 重写 `src/MILPGenerator.scala`

```scala
package ormcuopt

/**
 * 混合整数规划GPU代码生成器
 * 使用NVIDIA cuOpt MILP求解器
 */
object MILPGenerator extends CUOPTCodeGenerator {
  
  def generateMILPModel(formula: FormulaIR): String = {
    val modelName = sanitizeName(...)
    
    s"""
#!/usr/bin/env python3
# -*- coding: utf-8 -*-
\"\"\"
ORMDSL-CUOPT MILP Solver
Generated from: $modelName
Backend: NVIDIA cuOpt MILP
\"\"\"

import numpy as np
from cuopt_solver import MILPSolver

def main():
    solver = MILPSolver()
    
    # 添加变量 (区分整数/连续)
${generateMILPVariables(formula)}
    
    # 设置目标
${generateMILPObjective(formula)}
    
    # 添加约束
${generateMILPConstraints(formula)}
    
    # 求解
    result = solver.solve(time_limit=3600.0, gap=0.0001)
    
    print(f"Status: {result['status']}")
    print(f"Objective: {result['objective']}")
    print(f"Nodes explored: {result['nodes']}")
    print(f"MIP Gap: {result['mip_gap']:.6f}")
    
    return result
"""
  }
}
```

#### 3.5 重写 `src/RoutingGenerator.scala`

```scala
package ormcuopt

/**
 * 路径优化GPU代码生成器
 * 使用NVIDIA cuOpt Routing求解器
 */
object RoutingGenerator extends CUOPTCodeGenerator {
  
  def generateVRPModel(
    problemName: String,
    costMatrix: Array[Array[Double]],
    demands: Array[Double],
    capacities: Array[Double],
    nVehicles: Int
  ): String = {
    s"""
#!/usr/bin/env python3
# -*- coding: utf-8 -*-
\"\"\"
ORMDSL-CUOPT VRP Solver
Problem: $problemName
Backend: NVIDIA cuOpt Routing
\"\"\"

import numpy as np
from cuopt_solver import VRPSolver

def main():
    n_locations = ${costMatrix.length}
    n_vehicles = $nVehicles
    
    # 创建求解器
    solver = VRPSolver(n_locations=n_locations, n_vehicles=n_vehicles)
    
    # 设置成本矩阵
    costs = np.array([
${costMatrix.map(row => s"        [${row.mkString(", ")}]").mkString(",\n")}
    ], dtype=np.float64)
    solver.set_cost_matrix(costs)
    
    # 设置需求
    solver.set_demands(np.array([${demands.mkString(", ")}]))
    
    # 设置容量
    solver.set_capacities(np.array([${capacities.mkString(", ")}]))
    
    # 求解
    result = solver.solve(max_runtime=60.0)
    
    print(f"Status: {result['status']}")
    print(f"Total Cost: {result['total_cost']:.2f}")
    print(f"Routes: {result['num_routes']}")
    
    for i, route in enumerate(result['routes']):
        print(f"  Route {i+1}: {' -> '.join(map(str, route))} (cost: {result['route_costs'][i]:.2f})")
    
    return result

if __name__ == "__main__":
    main()
"""
  }
}
```

### Phase 4: 真实GPU单元测试 (第8-10天)

#### 3.6 创建 `tests/benchmark_test.py`

```python
#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
ORMDSL-CUOPT 真实GPU性能基准测试
Backend: NVIDIA cuOpt

测试用例:
1. LP: 大规模线性规划
2. MILP: 混合整数规划
3. VRP: 车辆路径问题
"""

import unittest
import numpy as np
import time
import sys
import os

# 添加src路径
sys.path.insert(0, os.path.join(os.path.dirname(__file__), '../src'))

from cuopt_solver import LPSolver, MILPSolver, VRPSolver


class TestLPSolver(unittest.TestCase):
    """LP求解器测试"""
    
    @classmethod
    def setUpClass(cls):
        """检查cuOpt是否可用"""
        try:
            from cuopt.linear_programming import problem
            cls.cuopt_available = True
        except ImportError:
            cls.cuopt_available = False
            print("Warning: cuOpt not installed. Run: pip install nvidia-cuopt")
    
    def test_simple_lp(self):
        """简单LP: min x + 2y, s.t. x + y <= 4, 2x + y <= 5"""
        if not self.cuopt_available:
            self.skipTest("cuOpt not available")
        
        solver = LPSolver(sense="minimize")
        solver.add_variable("x", lb=0)
        solver.add_variable("y", lb=0)
        solver.set_objective(np.array([1.0, 2.0]))
        solver.add_constraint(np.array([[1.0, 1.0], [2.0, 1.0]]), np.array([4.0, 5.0]))
        
        result = solver.solve(time_limit=10.0)
        
        # 验证解的正确性
        self.assertEqual(result['status'], 'optimal')
        self.assertIsNotNone(result['objective'])
        
        # 最优解应该是 x=1, y=3, obj=7
        x, y = result['solution'][0], result['solution'][1]
        self.assertAlmostEqual(x + y, 4.0, places=5)
        self.assertAlmostEqual(2*x + y, 5.0, places=5)
    
    def test_medium_lp(self):
        """中等规模LP: 100变量, 50约束"""
        if not self.cuopt_available:
            self.skipTest("cuOpt not available")
        
        n = 100
        m = 50
        
        np.random.seed(42)
        c = np.random.randn(n)
        A = np.random.randn(m, n)
        b = np.random.rand(m) * 100
        
        solver = LPSolver()
        for i in range(n):
            solver.add_variable(f"x{i}", lb=0)
        solver.set_objective(c)
        solver.add_constraint(A, b)
        
        start = time.time()
        result = solver.solve(time_limit=60.0)
        solve_time = time.time() - start
        
        self.assertEqual(result['status'], 'optimal')
        print(f"Medium LP (n={n}, m={m}): {solve_time:.4f}s")
    
    def test_large_lp(self):
        """大规模LP: 1000变量, 500约束"""
        if not self.cuopt_available:
            self.skipTest("cuOpt not available")
        
        n = 1000
        m = 500
        
        np.random.seed(42)
        c = np.random.randn(n)
        A = np.random.randn(m, n)
        b = np.random.rand(m) * 100
        
        solver = LPSolver()
        for i in range(n):
            solver.add_variable(f"x{i}", lb=0)
        solver.set_objective(c)
        solver.add_constraint(A, b)
        
        start = time.time()
        result = solver.solve(time_limit=300.0)
        solve_time = time.time() - start
        
        self.assertIn(result['status'], ['optimal', 'suboptimal'])
        print(f"Large LP (n={n}, m={m}): {solve_time:.4f}s")


class TestMILPSolver(unittest.TestCase):
    """MILP求解器测试"""
    
    @classmethod
    def setUpClass(cls):
        try:
            from cuopt.linear_programming import problem
            cls.cuopt_available = True
        except ImportError:
            cls.cuopt_available = False
    
    def test_assignment_problem(self):
        """指派问题: 3x3矩阵"""
        if not self.cuopt_available:
            self.skipTest("cuOpt not available")
        
        solver = MILPSolver()
        
        # 3个任务, 3个工人
        for i in range(3):
            solver.add_variable(f"x{i}", vtype='B')  # 二进制
        
        # 成本矩阵
        c = np.array([1, 2, 3, 4, 5, 6, 7, 8, 9])
        solver.set_objective(c)
        
        # 每行选择一个
        A = np.array([
            [1, 0, 0, 1, 0, 0, 1, 0, 0],  # 任务0
            [0, 1, 0, 0, 1, 0, 0, 1, 0],  # 任务1
            [0, 0, 1, 0, 0, 1, 0, 0, 1],  # 任务2
        ])
        solver.add_constraint(A, np.array([1.0, 1.0, 1.0]))
        
        result = solver.solve(time_limit=30.0)
        
        self.assertEqual(result['status'], 'optimal')
        print(f"Assignment Problem: obj={result['objective']}, nodes={result['nodes']}")
    
    def test_knapsack(self):
        """0-1背包问题"""
        if not self.cuopt_available:
            self.skipTest("cuOpt not available")
        
        values = np.array([60, 100, 120, 80, 50])
        weights = np.array([10, 20, 30, 40, 50])
        capacity = 50
        
        solver = MILPSolver()
        for i in range(5):
            solver.add_variable(f"x{i}", lb=0, ub=1, vtype='B')
        
        solver.set_objective(-values)  # max -> min negate
        solver.add_constraint(weights.reshape(1, -1), np.array([capacity]))
        
        result = solver.solve(time_limit=10.0)
        
        # 最优解应该是选择物品0,1,2: 价值=280, 重量=60
        selected = result['solution'] > 0.5
        total_value = values[selected].sum()
        total_weight = weights[selected].sum()
        
        self.assertLessEqual(total_weight, capacity)
        self.assertEqual(total_value, 280)  # 60+100+120


class TestVRPSolver(unittest.TestCase):
    """VRP求解器测试"""
    
    @classmethod
    def setUpClass(cls):
        try:
            from cuopt.routing import routing
            cls.cuopt_available = True
        except ImportError:
            cls.cuopt_available = False
    
    def test_cvrp_simple(self):
        """简单CVRP: 5客户, 2车辆"""
        if not self.cuopt_available:
            self.skipTest("cuOpt not available")
        
        # 6个位置 (0是仓库)
        costs = np.array([
            [0, 10, 15, 20, 12, 8],
            [10, 0, 18, 25, 14, 9],
            [15, 18, 0, 22, 16, 11],
            [20, 25, 22, 0, 19, 15],
            [12, 14, 16, 19, 0, 13],
            [8, 9, 11, 15, 13, 0]
        ])
        
        demands = np.array([0, 5, 3, 6, 4, 2])  # 仓库=0
        capacities = np.array([15, 15])
        
        solver = VRPSolver(n_locations=6, n_vehicles=2)
        solver.set_cost_matrix(costs)
        solver.set_demands(demands)
        solver.set_capacities(capacities)
        
        result = solver.solve(max_runtime=30.0)
        
        self.assertIn(result['status'], ['optimal', 'feasible'])
        self.assertIsNotNone(result['total_cost'])
        print(f"CVRP: cost={result['total_cost']:.2f}, routes={result['num_routes']}")
    
    def test_vrptw(self):
        """带时间窗VRP"""
        if not self.cuopt_available:
            self.skipTest("cuOpt not available")
        
        costs = np.array([
            [0, 10, 15, 20],
            [10, 0, 18, 25],
            [15, 18, 0, 22],
            [20, 25, 22, 0]
        ])
        
        demands = np.array([0, 3, 5, 4])
        capacities = np.array([20])
        time_windows = np.array([
            [0, 100],   # 仓库
            [10, 30],   # 客户1
            [20, 50],   # 客户2
            [30, 60],   # 客户3
        ])
        
        solver = VRPSolver(n_locations=4, n_vehicles=1)
        solver.set_cost_matrix(costs)
        solver.set_demands(demands)
        solver.set_capacities(capacities)
        solver.set_time_windows(time_windows)
        
        result = solver.solve(max_runtime=30.0)
        
        self.assertIn(result['status'], ['optimal', 'feasible'])


if __name__ == "__main__":
    # 检查GPU
    try:
        import pynvml
        pynvml.nvmlInit()
        device_count = pynvml.nvmlDeviceGetCount()
        print(f"Found {device_count} NVIDIA GPU(s)")
        pynvml.nvmlShutdown()
    except:
        print("Warning: Could not detect NVIDIA GPU")
    
    # 运行测试
    unittest.main(verbosity=2)
```

---

## 4. 实施时间表

| 阶段 | 任务 | 预计时间 | 状态 |
|------|------|----------|------|
| Phase 1 | IR到cuOpt转换层 | 2天 | ⏳ |
| Phase 2 | Python API集成 | 2天 | ⏳ |
| Phase 3 | LP生成器修复 | 1天 | ⏳ |
| Phase 4 | MILP生成器修复 | 1天 | ⏳ |
| Phase 5 | Routing生成器修复 | 1天 | ⏳ |
| Phase 6 | 真实GPU单元测试 | 2天 | ⏳ |
| Phase 7 | 文档更新 | 1天 | ⏳ |

---

## 5. 依赖项

```txt
# requirements.txt
nvidia-cuopt>=26.02
numpy>=1.21.0
jpype1>=1.4.0  # 用于Scala-Java互操作
```

---

## 6. 验证清单

- [ ] cuOpt安装验证
- [ ] LP简单问题验证
- [ ] MILP整数约束验证
- [ ] VRP路径优化验证
- [ ] 大规模问题性能测试
- [ ] 内存使用监控
- [ ] GPU利用率监控
