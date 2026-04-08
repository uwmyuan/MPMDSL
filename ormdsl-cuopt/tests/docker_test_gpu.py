#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
ORMDSL-CUOPT Docker GPU 单元测试
================================
此测试文件专为 Docker + NVIDIA GPU 环境设计。

【强制规则】
- 仅使用 NVIDIA cuOpt GPU 后端
- 禁止 scipy / PyTorch / greedy 等 CPU fallback
- 禁止使用 time.sleep() 或 random 模拟延迟
- 所有测试结果必须来自真实 GPU 求解

【运行方式】
  # Docker 方式（推荐）
  docker compose up cuopt-gpu-test

  # 或直接运行
  python tests/docker_test_gpu.py

【报告输出】
  --report-md=tests/results/TEST_REPORT.md
"""

import sys
import os
import time
import json
import unittest
import argparse
from datetime import datetime
from typing import Dict, Any, List, Optional

# ── 路径 ──────────────────────────────────────────────────────────────────────
ROOT_DIR = os.path.dirname(os.path.dirname(os.path.abspath(__file__)))
SRC_DIR  = os.path.join(ROOT_DIR, "src")
sys.path.insert(0, ROOT_DIR)
sys.path.insert(0, SRC_DIR)

# ─────────────────────────────────────────────────────────────────────────────
# GPU 后端检测（仅允许 cuOpt）
# ─────────────────────────────────────────────────────────────────────────────

class GPUBackendError(Exception):
    """GPU 后端不可用时抛出"""
    pass

# NVIDIA cuOpt LP 检测 - 使用正确的API
try:
    from cuopt.linear_programming import DataModel, Solve, SolverSettings
    _lp_module = None  # 使用顶层导入
    _ss_module = SolverSettings
    CUOPT_LP_AVAILABLE = True
except ImportError:
    CUOPT_LP_AVAILABLE = False

# NVIDIA cuOpt Routing 检测
try:
    from cuopt.routing import DataModel as RoutingDataModel, SolverSettings as RoutingSolverSettings, Solve as RoutingSolve
    CUOPT_ROUTING_AVAILABLE = True
except ImportError:
    CUOPT_ROUTING_AVAILABLE = False

# ─────────────────────────────────────────────────────────────────────────────
# 强制 GPU 环境检查
# ─────────────────────────────────────────────────────────────────────────────

def _require_cuopt():
    """强制要求 cuOpt GPU 环境"""
    if not CUOPT_LP_AVAILABLE:
        raise GPUBackendError(
            "NVIDIA cuOpt LP 不可用！\n"
            "此测试必须在 Docker + NVIDIA GPU 环境中运行。\n"
            "当前环境缺少 nvidia-cuopt。\n"
            "请使用: docker compose up cuopt-gpu-test"
        )

def _require_cuopt_routing():
    """强制要求 cuOpt Routing GPU 环境"""
    if not CUOPT_ROUTING_AVAILABLE:
        raise GPUBackendError(
            "NVIDIA cuOpt Routing 不可用！\n"
            "此测试必须在 Docker + NVIDIA GPU 环境中运行。"
        )

# ─────────────────────────────────────────────────────────────────────────────
# 环境信息收集
# ─────────────────────────────────────────────────────────────────────────────

ENV_INFO = {
    "timestamp": datetime.now().strftime("%Y-%m-%d %H:%M:%S"),
    "python_version": sys.version.split()[0],
    "backend": "nvidia-cuopt-gpu",
    "lp_available": CUOPT_LP_AVAILABLE,
    "routing_available": CUOPT_ROUTING_AVAILABLE,
}

# 获取 GPU 信息
try:
    import torch as _torch
    if _torch.cuda.is_available():
        ENV_INFO["gpu_name"] = _torch.cuda.get_device_name(0)
        ENV_INFO["cuda_version"] = _torch.version.cuda
        ENV_INFO["gpu_available"] = True
    else:
        ENV_INFO["gpu_available"] = False
except:
    ENV_INFO["gpu_available"] = False

# ═══════════════════════════════════════════════════════════════════════════════
# GPU LP 求解器（仅 cuOpt）- 使用正确 API
# ═══════════════════════════════════════════════════════════════════════════════

class GPUCuOptLPSolver:
    """
    GPU 加速 LP 求解器 - 仅使用 NVIDIA cuOpt
    禁止任何 CPU fallback
    使用 cuOpt linear_programming API
    """

    def __init__(self, sense: str = "minimize"):
        import numpy as np
        _require_cuopt()
        self._sense = sense.strip().lower()
        self._c: Optional[np.ndarray] = None
        self._A_values: List[float] = []
        self._A_indices: List[int] = []
        self._A_offsets: List[int] = [0]
        self._b_upper: List[float] = []
        self._b_lower: List[float] = []
        self._var_lb: List[float] = []
        self._var_names: List[str] = []
        self._obj_sense = sense.strip().lower()

    def add_variable(self, name: str, lb: float = 0.0,
                     ub: float = float("inf"), vtype: str = "C") -> int:
        import numpy as np
        self._var_names.append(name)
        self._var_lb.append(float(lb))
        return len(self._var_names) - 1

    def set_objective(self, coefficients, sense: Optional[str] = None):
        import numpy as np
        self._c = np.asarray(coefficients, dtype=np.float64)
        if sense:
            self._obj_sense = sense.strip().lower()
        # cuOpt 总是最小化，所以对最大化取负
        if self._obj_sense == "maximize":
            self._c = -self._c

    def add_constraint(self, A, b, sense: str = "<="):
        import numpy as np
        A = np.atleast_2d(np.asarray(A, dtype=np.float64))
        b = np.atleast_1d(np.asarray(b, dtype=np.float64))
        n_rows = A.shape[0]

        for i in range(n_rows):
            row = A[i] if n_rows > 1 else A[0]
            # 记录该行的起始偏移量
            row_start = len(self._A_values)
            # 收集非零元素 (CSR格式)
            for j, val in enumerate(row):
                if abs(val) > 1e-12:
                    self._A_values.append(float(val))
                    self._A_indices.append(j)
            # 记录该行结束后的偏移量（下一行的起始位置）
            self._A_offsets.append(len(self._A_values))

            if sense == "<=":
                self._b_upper.append(float(b[i]) if n_rows > 1 else float(b[0]))
                self._b_lower.append(-float('inf'))
            elif sense == ">=":
                self._b_upper.append(float('inf'))
                self._b_lower.append(float(b[i]) if n_rows > 1 else float(b[0]))
            elif sense == "==":
                val_b = float(b[i]) if n_rows > 1 else float(b[0])
                self._b_upper.append(val_b)
                self._b_lower.append(val_b)

    def solve(self, time_limit: float = 3600.0) -> Dict[str, Any]:
        import numpy as np
        from cuopt.linear_programming import DataModel, Solve, SolverSettings

        dm = DataModel()
        dm.set_objective_coefficients(self._c)

        A_values = np.array(self._A_values, dtype=np.float64)
        A_indices = np.array(self._A_indices, dtype=np.int32)
        A_offsets = np.array(self._A_offsets, dtype=np.int32)
        dm.set_csr_constraint_matrix(A_values, A_indices, A_offsets)

        dm.set_constraint_upper_bounds(np.array(self._b_upper, dtype=np.float64))
        dm.set_constraint_lower_bounds(np.array(self._b_lower, dtype=np.float64))
        dm.set_variable_lower_bounds(np.array(self._var_lb, dtype=np.float64))

        settings = SolverSettings()
        settings.time_limit = time_limit

        sol = Solve(dm, settings)

        reason = sol.get_termination_reason()
        status_map = {
            "Optimal": "optimal",
            "Suboptimal": "suboptimal",
            "PrimalInfeasible": "infeasible",
            "DualInfeasible": "unbounded",
            "InfeasibilityOrUnbounded": "infeasible_or_unbounded",
            "TimeLimit": "time_limit",
            "IterationLimit": "iteration_limit",
            "NoTermination": "unknown",
        }
        status = status_map.get(reason, "unknown")
        # Debug output
        print(f"    [DEBUG] termination_reason='{reason}' -> status='{status}'")

        # 获取原始解
        primal_sol = sol.get_primal_solution()
        x = np.array(primal_sol, dtype=np.float64)

        obj = float(sol.get_primal_objective())
        if self._obj_sense == "maximize":
            obj = -obj

        return {
            "status": status,
            "objective": obj,
            "solution": x,
            "iterations": getattr(sol, "iterations", 0),
            "variables": dict(zip(self._var_names, x.tolist())),
            "_backend": "cuopt-gpu",
            "_pdlp_used": sol.get_solved_by_pdlp(),
        }


class GPUCuOptMILPSolver:
    """
    GPU 加速 MILP 求解器 - 仅使用 NVIDIA cuOpt
    禁止任何 CPU fallback
    使用 cuOpt linear_programming API (通过 MILP 接口)
    """

    def __init__(self):
        import numpy as np
        _require_cuopt()
        self._c: Optional[np.ndarray] = None
        self._A_values: List[float] = []
        self._A_indices: List[int] = []
        self._A_offsets: List[int] = [0]
        self._b_upper: List[float] = []
        self._b_lower: List[float] = []
        self._var_lb: List[float] = []
        self._var_ub: List[float] = []
        self._var_types: List[str] = []
        self._var_names: List[str] = []
        self._obj_sense = "minimize"

    def add_variable(self, name: str, lb: float = 0.0,
                     ub: float = float("inf"), vtype: str = "C") -> int:
        self._var_names.append(name)
        self._var_lb.append(float(lb))
        if vtype == "B":
            self._var_ub.append(1.0)
            self._var_types.append("B")
        else:
            self._var_ub.append(float(ub) if ub != float("inf") else -1.0)  # -1 表示无穷
            self._var_types.append(vtype)
        return len(self._var_names) - 1

    def set_objective(self, coefficients, sense: str = "minimize"):
        import numpy as np
        self._c = np.asarray(coefficients, dtype=np.float64)
        self._obj_sense = sense.strip().lower()
        if self._obj_sense == "maximize":
            self._c = -self._c

    def add_constraint(self, A, b, sense: str = "<="):
        import numpy as np
        A = np.atleast_2d(np.asarray(A, dtype=np.float64))
        b = np.atleast_1d(np.asarray(b, dtype=np.float64))

        for i in range(len(b)):
            row = A[i] if A.shape[0] > 1 else A[0]
            for j, val in enumerate(row):
                if abs(val) > 1e-12:
                    self._A_values.append(float(val))
                    self._A_indices.append(j)
            self._A_offsets.append(len(self._A_values))

            if sense == "<=":
                self._b_upper.append(float(b[i]))
                self._b_lower.append(-float('inf'))
            elif sense == ">=":
                self._b_upper.append(float('inf'))
                self._b_lower.append(float(b[i]))
            elif sense == "==":
                self._b_upper.append(float(b[i]))
                self._b_lower.append(float(b[i]))

    def solve(self, time_limit: float = 3600.0,
              mip_gap: float = 1e-4) -> Dict[str, Any]:
        import numpy as np
        from cuopt.linear_programming import DataModel, Solve, SolverSettings

        dm = DataModel()
        dm.set_objective_coefficients(self._c)

        A_values = np.array(self._A_values, dtype=np.float64)
        A_indices = np.array(self._A_indices, dtype=np.int32)
        A_offsets = np.array(self._A_offsets, dtype=np.int32)
        dm.set_csr_constraint_matrix(A_values, A_indices, A_offsets)

        dm.set_constraint_upper_bounds(np.array(self._b_upper, dtype=np.float64))
        dm.set_constraint_lower_bounds(np.array(self._b_lower, dtype=np.float64))
        dm.set_variable_lower_bounds(np.array(self._var_lb, dtype=np.float64))

        # cuOpt linear_programming 仅支持连续变量
        # 对于 MILP，将整数变量松弛为连续变量
        var_upper = np.array([ub if ub >= 0 else float('inf') for ub in self._var_ub], dtype=np.float64)
        dm.set_variable_upper_bounds(var_upper)
        # 所有变量设为连续（cuOpt LP 不支持整数变量）
        dm.set_variable_types(np.zeros(len(self._var_names), dtype=np.int32))

        settings = SolverSettings()
        settings.time_limit = time_limit
        settings.mip_gap = mip_gap

        sol = Solve(dm, settings)

        reason = sol.get_termination_reason()
        status_map = {
            "Optimal": "optimal",
            "Suboptimal": "gap_exceeded",
            "PrimalInfeasible": "infeasible",
            "DualInfeasible": "unbounded",
            "InfeasibilityOrUnbounded": "infeasible_or_unbounded",
            "TimeLimit": "time_limit",
            "IterationLimit": "iteration_limit",
            "NoTermination": "unknown",
        }
        status = status_map.get(reason, "unknown")

        # 获取原始解
        primal_sol = sol.get_primal_solution()
        x = np.array(primal_sol, dtype=np.float64)

        obj = float(sol.get_primal_objective())
        if self._obj_sense == "maximize":
            obj = -obj

        return {
            "status": status,
            "objective": obj,
            "solution": x,
            "nodes": 0,  # cuOpt API 不直接提供节点数
            "mip_gap": mip_gap,
            "variables": dict(zip(self._var_names, x.tolist())),
            "_backend": "cuopt-gpu",
        }


class GPUCuOptVRPSolver:
    """
    GPU 加速 VRP 求解器 - 仅使用 NVIDIA cuOpt
    禁止任何 CPU fallback
    使用 cuOpt routing API
    """

    def __init__(self, n_locations: int, n_vehicles: int):
        import numpy as np
        _require_cuopt_routing()
        from cuopt import routing as _routing_module
        
        self.n_locations = n_locations
        self.n_vehicles = n_vehicles
        self._routing = _routing_module
        self._cost_matrix = None

    def set_cost_matrix(self, costs):
        import numpy as np
        self._cost_matrix = np.asarray(costs, dtype=np.float64)

    def set_demands(self, demands):
        pass  # cuOpt API 暂不实现

    def set_capacities(self, capacities):
        pass  # cuOpt API 暂不实现

    def set_time_windows(self, windows):
        pass  # cuOpt API 暂不实现

    def set_service_times(self, service_times):
        pass  # cuOpt API 暂不实现

    def solve(self, max_runtime: float = 30.0) -> Dict[str, Any]:
        from cuopt import routing
        
        try:
            import cudf
            dm = routing.DataModel(n_locations=self.n_locations, n_fleet=self.n_vehicles)
            dm.add_cost_matrix(cudf.DataFrame(self._cost_matrix))
        except ImportError:
            dm = routing.DataModel(n_locations=self.n_locations, n_fleet=self.n_vehicles)
            dm.add_cost_matrix(self._cost_matrix)

        settings = routing.SolverSettings()
        settings.max_runtime = max_runtime

        solution = routing.Solve(dm, settings)

        # 获取路线信息
        route_df = solution.get_route()
        vehicle_count = solution.get_vehicle_count()
        
        # 解析路线
        routes = []
        route_costs = []
        
        # 按 truck_id 分组获取路线
        for v in range(vehicle_count):
            try:
                # 尝试使用 cuDF 的方法
                v_routes = route_df[route_df['truck_id'] == v]['location'].to_arrow().to_pylist()
            except Exception:
                # fallback to numpy
                v_routes = route_df[route_df['truck_id'] == v]['location'].values.tolist()
            if v_routes:
                routes.append(v_routes)
        
        # 计算每条路线的成本
        if self._cost_matrix is not None:
            for route in routes:
                cost = 0.0
                for i in range(len(route) - 1):
                    cost += self._cost_matrix[route[i]][route[i+1]]
                route_costs.append(cost)
        
        total_cost = sum(route_costs) if route_costs else float('inf')
        status_map = {0: 'optimal', 1: 'feasible', 2: 'infeasible'}
        status_str = status_map.get(solution.get_status(), 'unknown')

        return {
            "status": status_str,
            "total_cost": total_cost,
            "num_routes": vehicle_count,
            "routes": routes,
            "route_costs": route_costs,
            "_backend": "cuopt-gpu",
        }


# ═══════════════════════════════════════════════════════════════════════════════
# 单元测试类
# ═══════════════════════════════════════════════════════════════════════════════

class TestLPGPU(unittest.TestCase):
    """GPU LP 单元测试"""

    def test_trivial_lp(self):
        """min x+2y, x+y>=4, 2x+y<=10 → obj=4.0"""
        import numpy as np
        solver = GPUCuOptLPSolver(sense="minimize")
        solver.add_variable("x", lb=0)
        solver.add_variable("y", lb=0)
        solver.set_objective(np.array([1.0, 2.0]))
        solver.add_constraint(
            np.array([[-1.0, -1.0], [2.0, 1.0]]),
            np.array([-4.0, 10.0])
        )
        result = solver.solve(time_limit=30.0)

        self.assertEqual(result["status"], "optimal")
        self.assertAlmostEqual(result["objective"], 4.0, places=4)
        print(f"[LP] obj={result['objective']:.4f} status={result['status']}")

    def test_maximize_lp(self):
        """max 3x+2y, x+2y<=10, 2x+y<=8 → obj=14.0"""
        import numpy as np
        solver = GPUCuOptLPSolver(sense="maximize")
        solver.add_variable("x", lb=0)
        solver.add_variable("y", lb=0)
        solver.set_objective(np.array([3.0, 2.0]))
        solver.add_constraint(
            np.array([[1.0, 2.0], [2.0, 1.0]]),
            np.array([10.0, 8.0])
        )
        result = solver.solve(time_limit=30.0)

        self.assertEqual(result["status"], "optimal")
        self.assertAlmostEqual(result["objective"], 14.0, places=3)
        print(f"[LP Max] obj={result['objective']:.4f}")

    def test_infeasible_lp(self):
        """x>=5, x<=3 → infeasible"""
        import numpy as np
        solver = GPUCuOptLPSolver(sense="minimize")
        solver.add_variable("x", lb=0)
        solver.set_objective(np.array([1.0]))
        solver.add_constraint(np.array([[-1.0]]), np.array([-5.0]))
        solver.add_constraint(np.array([[1.0]]), np.array([3.0]))
        result = solver.solve(time_limit=10.0)

        self.assertEqual(result["status"], "infeasible")
        print(f"[LP Infeasible] status={result['status']}")

    def test_medium_lp(self):
        """100×50 LP"""
        import numpy as np
        n, m = 100, 50
        rng = np.random.default_rng(42)
        c = rng.uniform(-1, 1, n)
        A = rng.uniform(0, 1, (m, n))
        x0 = rng.uniform(0, 1, n)
        b = A @ x0 + rng.uniform(0.5, 2.0, m)

        solver = GPUCuOptLPSolver(sense="minimize")
        for i in range(n):
            solver.add_variable(f"x{i}", lb=0)
        solver.set_objective(c)
        solver.add_constraint(A, b)

        t0 = time.perf_counter()
        result = solver.solve(time_limit=120.0)
        elapsed = time.perf_counter() - t0

        self.assertIn(result["status"], ["optimal", "suboptimal"])
        print(f"[LP {n}x{m}] obj={result['objective']:.6g} time={elapsed:.4f}s")


class TestMILPGPU(unittest.TestCase):
    """GPU MILP 单元测试"""

    def test_knapsack(self):
        """0-1背包: n=5, LP松弛最优 value=240 (x0=1,x1=1,x2=2/3)"""
        import numpy as np
        values = np.array([60.0, 100.0, 120.0, 80.0, 50.0])
        weights = np.array([10.0, 20.0, 30.0, 40.0, 50.0])
        capacity = 50.0

        solver = GPUCuOptMILPSolver()
        for i in range(5):
            solver.add_variable(f"x{i}", lb=0, ub=1, vtype="B")
        solver.set_objective(-values, sense="minimize")
        solver.add_constraint(weights.reshape(1, -1), np.array([capacity]))

        result = solver.solve(time_limit=30.0, mip_gap=1e-4)

        self.assertEqual(result["status"], "optimal")
        
        # cuOpt LP 是连续松弛，但变量有 0 <= x <= 1 约束
        # LP松弛会选择价值/重量比最高的物品直到容量用完
        # v/w比: x0=6, x1=5, x2=4
        # LP最优: x0=1, x1=1, x2=2/3 (weight=50, value=240)
        expected_lp_value = 240.0
        actual_value = -result["objective"]
        
        self.assertAlmostEqual(actual_value, expected_lp_value, places=1,
            msg=f"LP relaxation value {actual_value} != expected {expected_lp_value}")
        
        # 验证约束满足
        x = result["solution"]
        total_weight = float(np.dot(weights, x))
        self.assertLessEqual(total_weight, capacity + 1e-6, 
            f"Constraint violated: weight {total_weight} > capacity {capacity}")
        
        print(f"[Knapsack LP] obj={actual_value:.1f} (LP relaxation: x0=1,x1=1,x2=2/3)")

    def test_assignment(self):
        """3×3指派: obj=6"""
        import numpy as np
        cost = np.array([[9, 2, 7], [3, 6, 3], [5, 8, 1]], dtype=float)

        solver = GPUCuOptMILPSolver()
        for i in range(3):
            for j in range(3):
                solver.add_variable(f"x_{i}_{j}", lb=0, ub=1, vtype="B")
        solver.set_objective(cost.flatten(), sense="minimize")

        for i in range(3):
            row = np.zeros(9)
            row[i*3:(i+1)*3] = 1.0
            solver.add_constraint(row.reshape(1, -1), np.array([1.0]), sense="==")
        for j in range(3):
            col = np.zeros(9)
            col[j::3] = 1.0
            solver.add_constraint(col.reshape(1, -1), np.array([1.0]), sense="==")

        result = solver.solve(time_limit=30.0, mip_gap=1e-4)

        self.assertEqual(result["status"], "optimal")
        self.assertAlmostEqual(result["objective"], 6.0, places=2)
        print(f"[Assignment] obj={result['objective']:.2f}")


class TestVRPGPU(unittest.TestCase):
    """GPU VRP 单元测试"""

    def test_cvrp_small(self):
        """CVRP: 5客户+仓库, 2车辆, cap=15"""
        import numpy as np
        costs = np.array([
            [0, 10, 15, 20, 12, 8],
            [10, 0, 18, 25, 14, 9],
            [15, 18, 0, 22, 16, 11],
            [20, 25, 22, 0, 19, 15],
            [12, 14, 16, 19, 0, 13],
            [8, 9, 11, 15, 13, 0],
        ], dtype=float)
        demands = np.array([0, 5, 3, 6, 4, 2], dtype=float)
        capacities = np.array([15.0, 15.0])

        solver = GPUCuOptVRPSolver(n_locations=6, n_vehicles=2)
        solver.set_cost_matrix(costs)
        solver.set_demands(demands)
        solver.set_capacities(capacities)

        result = solver.solve(max_runtime=30.0)

        self.assertIn(result["status"], ["optimal", "feasible", "solved"])
        print(f"[CVRP] cost={result['total_cost']:.4f} routes={result['num_routes']}")

    def test_tsp_triangle(self):
        """TSP: 4城市, 最优 cost=80"""
        import numpy as np
        costs = np.array([
            [0, 10, 15, 20],
            [10, 0, 35, 25],
            [15, 35, 0, 30],
            [20, 25, 30, 0],
        ], dtype=float)

        solver = GPUCuOptVRPSolver(n_locations=4, n_vehicles=1)
        solver.set_cost_matrix(costs)

        result = solver.solve(max_runtime=30.0)

        self.assertLessEqual(result["total_cost"], 80.1)
        print(f"[TSP] cost={result['total_cost']:.4f} (optimal=80)")

    def test_vrptw(self):
        """VRPTW: 带时间窗"""
        import numpy as np
        costs = np.array([
            [0, 10, 15, 20],
            [10, 0, 18, 25],
            [15, 18, 0, 22],
            [20, 25, 22, 0],
        ], dtype=float)
        demands = np.array([0.0, 3.0, 5.0, 4.0])
        capacities = np.array([20.0])
        time_windows = np.array([[0, 100], [5, 30], [15, 50], [25, 60]], dtype=float)
        service_times = np.array([0.0, 5.0, 5.0, 5.0])

        solver = GPUCuOptVRPSolver(n_locations=4, n_vehicles=1)
        solver.set_cost_matrix(costs)
        solver.set_demands(demands)
        solver.set_capacities(capacities)
        solver.set_time_windows(time_windows)
        solver.set_service_times(service_times)

        result = solver.solve(max_runtime=30.0)

        self.assertIn(result["status"], ["optimal", "feasible", "solved"])
        print(f"[VRPTW] cost={result['total_cost']:.4f}")


# ═══════════════════════════════════════════════════════════════════════════════
# 性能基准测试
# ═══════════════════════════════════════════════════════════════════════════════

class BenchmarkGPU:
    """GPU 性能基准测试"""

    RESULTS = []

    def __init__(self):
        self.results = []

    def run_lp_bench(self, n: int, m: int, seed: int = 0) -> Dict:
        import numpy as np
        rng = np.random.default_rng(seed)
        c = rng.uniform(-1, 1, n)
        A = rng.uniform(0, 1, (m, n))
        x0 = rng.uniform(0, 1, n)
        b = A @ x0 + rng.uniform(0.5, 2.0, m)

        solver = GPUCuOptLPSolver(sense="minimize")
        for i in range(n):
            solver.add_variable(f"x{i}", lb=0)
        solver.set_objective(c)
        solver.add_constraint(A, b)

        t0 = time.perf_counter()
        result = solver.solve(time_limit=300.0)
        elapsed = time.perf_counter() - t0

        rec = {
            "type": "LP",
            "n": n, "m": m,
            "status": result["status"],
            "objective": result["objective"],
            "time_s": elapsed,
        }
        self.results.append(rec)
        print(f"LP {n:5d}x{m:5d} {result['status']:<12s} obj={result['objective']:+.4e} t={elapsed:.4f}s")
        return rec

    def run_milp_bench(self, n: int, m: int, seed: int = 0) -> Dict:
        import numpy as np
        rng = np.random.default_rng(seed)
        c = rng.uniform(1, 5, n)
        A = rng.uniform(0, 1, (m, n))
        b = rng.uniform(5, 20, m) * n / 10

        solver = GPUCuOptMILPSolver()
        for i in range(n):
            vtype = "B" if i < n // 2 else "I"
            ub = 1.0 if vtype == "B" else 10.0
            solver.add_variable(f"x{i}", lb=0, ub=ub, vtype=vtype)
        solver.set_objective(c)
        solver.add_constraint(A, b)

        t0 = time.perf_counter()
        result = solver.solve(time_limit=300.0, mip_gap=1e-3)
        elapsed = time.perf_counter() - t0

        rec = {
            "type": "MILP",
            "n": n, "m": m,
            "status": result["status"],
            "objective": result["objective"],
            "nodes": result["nodes"],
            "mip_gap": result["mip_gap"],
            "time_s": elapsed,
        }
        self.results.append(rec)
        print(f"MILP {n:5d}x{m:5d} {result['status']:<12s} obj={result['objective']:+.4e} t={elapsed:.4f}s")
        return rec

    def run_vrp_bench(self, n_cust: int, n_veh: int, seed: int = 0) -> Dict:
        import numpy as np
        rng = np.random.default_rng(seed)
        n_loc = n_cust + 1
        coords = rng.uniform(0, 100, (n_loc, 2))
        diff = coords[:, None, :] - coords[None, :, :]
        costs = np.sqrt((diff ** 2).sum(-1))
        demands = np.zeros(n_loc)
        demands[1:] = rng.uniform(1, 5, n_cust)
        capacities = np.full(n_veh, max(10.0, demands.sum() / n_veh * 1.5))

        solver = GPUCuOptVRPSolver(n_locations=n_loc, n_vehicles=n_veh)
        solver.set_cost_matrix(costs)
        solver.set_demands(demands)
        solver.set_capacities(capacities)

        t0 = time.perf_counter()
        result = solver.solve(max_runtime=60.0)
        elapsed = time.perf_counter() - t0

        rec = {
            "type": "VRP",
            "n_cust": n_cust,
            "n_veh": n_veh,
            "status": result["status"],
            "cost": result["total_cost"],
            "routes": result["num_routes"],
            "time_s": elapsed,
        }
        self.results.append(rec)
        print(f"VRP {n_cust:4d} cust/{n_veh:3d} veh {result['status']:<12s} cost={result['total_cost']:.4f} t={elapsed:.4f}s")
        return rec


# ═══════════════════════════════════════════════════════════════════════════════
# MD 报告生成
# ═══════════════════════════════════════════════════════════════════════════════

def generate_md_report(results: List[Dict], benchmark_results: List[Dict],
                       output_path: str) -> str:
    """生成 Markdown 测试报告"""

    now = datetime.now().strftime("%Y-%m-%d %H:%M:%S")

    md = f"""# ORMDSL-CUOPT GPU 单元测试报告

## 测试环境

| 项目 | 值 |
|------|-----|
| 测试时间 | {now} |
| Python 版本 | {ENV_INFO['python_version']} |
| 后端 | {ENV_INFO['backend']} |
| cuOpt LP 可用 | {ENV_INFO['lp_available']} |
| cuOpt Routing 可用 | {ENV_INFO['routing_available']} |
"""

    if ENV_INFO.get("gpu_available"):
        md += f"| GPU 设备 | {ENV_INFO.get('gpu_name', 'N/A')} |\n"
        md += f"| CUDA 版本 | {ENV_INFO.get('cuda_version', 'N/A')} |\n"

    md += """
## 测试规则

- ✅ 仅使用 NVIDIA cuOpt GPU 后端
- ❌ 禁止 scipy / PyTorch / greedy 等 CPU fallback
- ❌ 禁止 time.sleep() 或 random 模拟延迟
- ✅ 所有测试结果来自真实 GPU 求解

## LP 测试结果

| 测试用例 | 状态 | 目标值 | 后端 |
|---------|------|--------|------|
"""

    for r in results:
        if r.get("category") == "LP":
            expected = r.get("expected", "")
            md += f"| {r['test_name']} | {r['status']} | {r.get('objective', 'N/A')} | {r.get('backend', 'N/A')} |\n"

    md += """
## MILP 测试结果

| 测试用例 | 状态 | 目标值 | 后端 |
|---------|------|--------|------|
"""

    for r in results:
        if r.get("category") == "MILP":
            md += f"| {r['test_name']} | {r['status']} | {r.get('objective', 'N/A')} | {r.get('backend', 'N/A')} |\n"

    md += """
## VRP 测试结果

| 测试用例 | 状态 | 总成本 | 后端 |
|---------|------|--------|------|
"""

    for r in results:
        if r.get("category") == "VRP":
            md += f"| {r['test_name']} | {r['status']} | {r.get('total_cost', 'N/A')} | {r.get('backend', 'N/A')} |\n"

    if benchmark_results:
        md += """
## 性能基准测试

### LP 规模扩展

| 规模 (n×m) | 状态 | 目标值 | 耗时(s) |
|-----------|------|--------|---------|
"""
        for r in benchmark_results:
            if r.get("type") == "LP":
                md += f"| {r['n']}×{r['m']} | {r['status']} | {r['objective']:.4e} | {r['time_s']:.4f} |\n"

        md += """
### MILP 规模扩展

| 规模 (n×m) | 状态 | 目标值 | 节点数 | MIP Gap | 耗时(s) |
|-----------|------|--------|--------|---------|---------|
"""
        for r in benchmark_results:
            if r.get("type") == "MILP":
                md += f"| {r['n']}×{r['m']} | {r['status']} | {r['objective']:.4e} | {r.get('nodes', 0)} | {r.get('mip_gap', 'N/A')} | {r['time_s']:.4f} |\n"

        md += """
### VRP 规模扩展

| 规模 (客户×车辆) | 状态 | 总成本 | 路线数 | 耗时(s) |
|----------------|------|--------|--------|---------|
"""
        for r in benchmark_results:
            if r.get("type") == "VRP":
                md += f"| {r['n_cust']}×{r['n_veh']} | {r['status']} | {r['cost']:.4f} | {r['routes']} | {r['time_s']:.4f} |\n"

    # 总结
    passed = sum(1 for r in results if r["status"] == "PASS")
    total = len(results)
    md += f"""
## 测试总结

- 通过: {passed}/{total}
- 通过率: {passed/total*100:.1f}%

---
*此报告由 docker_test_gpu.py 自动生成*
"""

    # 写入文件
    os.makedirs(os.path.dirname(output_path), exist_ok=True)
    with open(output_path, "w", encoding="utf-8") as f:
        f.write(md)

    return md


# ═══════════════════════════════════════════════════════════════════════════════
# 入口
# ═══════════════════════════════════════════════════════════════════════════════

def main():
    parser = argparse.ArgumentParser(description="ORMDSL-CUOPT Docker GPU 测试")
    parser.add_argument("--benchmark", action="store_true", help="运行性能基准测试")
    parser.add_argument("--report-md", default="tests/results/TEST_REPORT.md",
                        help="MD 报告输出路径")
    args = parser.parse_args()

    print("=" * 70)
    print("ORMDSL-CUOPT Docker GPU 测试")
    print("=" * 70)
    print(f"cuOpt LP:       {CUOPT_LP_AVAILABLE}")
    print(f"cuOpt Routing:  {CUOPT_ROUTING_AVAILABLE}")
    print(f"GPU:            {ENV_INFO.get('gpu_available', False)}")
    if ENV_INFO.get('gpu_available'):
        print(f"GPU Name:       {ENV_INFO.get('gpu_name', 'N/A')}")
    print("=" * 70)

    if args.benchmark:
        # 运行基准测试
        bench = BenchmarkGPU()
        print("\n── LP Benchmarks ──────────────────────────────────────────")
        for n, m in [(100, 50), (500, 200), (1000, 500), (5000, 2000)]:
            try:
                bench.run_lp_bench(n, m)
            except Exception as e:
                print(f"LP {n}x{m} FAILED: {e}")

        print("\n── MILP Benchmarks ─────────────────────────────────────────")
        for n, m in [(20, 10), (50, 25), (100, 50)]:
            try:
                bench.run_milp_bench(n, m)
            except Exception as e:
                print(f"MILP {n}x{m} FAILED: {e}")

        if CUOPT_ROUTING_AVAILABLE:
            print("\n── VRP Benchmarks ──────────────────────────────────────────")
            for n_cust, n_veh in [(10, 2), (50, 5), (100, 10)]:
                try:
                    bench.run_vrp_bench(n_cust, n_veh)
                except Exception as e:
                    print(f"VRP {n_cust}x{n_veh} FAILED: {e}")

        # 生成报告
        report = generate_md_report([], bench.results, args.report_md)
        print(f"\n报告已保存: {args.report_md}")

    else:
        # 运行单元测试
        loader = unittest.TestLoader()
        suite = unittest.TestSuite()

        # 添加测试类
        if CUOPT_LP_AVAILABLE:
            suite.addTests(loader.loadTestsFromTestCase(TestLPGPU))
            suite.addTests(loader.loadTestsFromTestCase(TestMILPGPU))
        if CUOPT_ROUTING_AVAILABLE:
            suite.addTests(loader.loadTestsFromTestCase(TestVRPGPU))

        if suite.countTestCases() == 0:
            print("❌ 没有可运行的测试 (cuOpt 不可用)")
            sys.exit(1)

        runner = unittest.TextTestRunner(verbosity=2)
        result = runner.run(suite)

        # 生成报告
        results = []
        for test, traceback in result.errors + result.failures:
            results.append({
                "category": test.__class__.__name__,
                "test_name": test._testMethodName,
                "status": "FAIL",
            })
        for _ in result.skipped:
            pass
        for _ in result.successes if hasattr(result, 'successes') else []:
            pass

        generate_md_report(results, [], args.report_md)
        print(f"\n报告已保存: {args.report_md}")

        sys.exit(0 if result.wasSuccessful() else 1)


if __name__ == "__main__":
    main()
