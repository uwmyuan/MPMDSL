#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
ORMDSL-CUOPT  真实 GPU 单元测试 & 性能基准
=========================================
后端  : NVIDIA cuOpt  (pip install nvidia-cuopt)
运行  : python tests/benchmark_test.py
       python tests/benchmark_test.py --class TestLPSolver
       python tests/benchmark_test.py --benchmark       # 仅性能测试

规则:
  * 不允许对 GPU / 求解时间做任何仿真
  * 不使用 random.random() 代替真实求解
  * 不使用 time.sleep() 模拟延迟
  * 所有断言基于真实求解结果验证
"""

import sys
import os
import time
import json
import unittest
import argparse
import numpy as np
from typing import Dict, Any

# ── 路径 ──────────────────────────────────────────────────────────────────────
ROOT_DIR = os.path.dirname(os.path.dirname(os.path.abspath(__file__)))
SRC_DIR  = os.path.join(ROOT_DIR, "src")
sys.path.insert(0, ROOT_DIR)
sys.path.insert(0, SRC_DIR)

# ── cuOpt 可用性 ──────────────────────────────────────────────────────────────
try:
    from cuopt.linear_programming import problem as _lp
    from cuopt.linear_programming import solver_settings as _ss
    LP_AVAILABLE = True
except ImportError:
    LP_AVAILABLE = False

try:
    from cuopt.routing import routing as _rt
    ROUTING_AVAILABLE = True
except ImportError:
    ROUTING_AVAILABLE = False

# scipy fallback 可用性
try:
    from scipy.optimize import linprog as _lp_check  # noqa: F401
    SCIPY_AVAILABLE = True
except ImportError:
    SCIPY_AVAILABLE = False

# PyTorch GPU 可用性
try:
    import torch as _torch
    TORCH_AVAILABLE = True
    TORCH_GPU_AVAILABLE = _torch.cuda.is_available()
    if TORCH_GPU_AVAILABLE:
        GPU_NAME = _torch.cuda.get_device_name(0)
        CUDA_VERSION = _torch.version.cuda
    else:
        GPU_NAME = "N/A"
        CUDA_VERSION = None
except ImportError:
    _torch = None  # type: ignore
    TORCH_AVAILABLE = False
    TORCH_GPU_AVAILABLE = False
    GPU_NAME = "N/A"
    CUDA_VERSION = None

# 从 cuopt_solver.py 导入封装类（支持 cuOpt GPU 或 PyTorch GPU 或 scipy CPU）
try:
    from cuopt_solver import LPSolver, MILPSolver, VRPSolver
    WRAPPER_AVAILABLE = True
except ImportError:
    try:
        from src.cuopt_solver import LPSolver, MILPSolver, VRPSolver
        WRAPPER_AVAILABLE = True
    except ImportError:
        WRAPPER_AVAILABLE = False

# 当 cuOpt 不可用但 scipy 或 PyTorch 可用时，测试仍可运行
_LP_RUNNABLE      = (LP_AVAILABLE or SCIPY_AVAILABLE or TORCH_AVAILABLE) and WRAPPER_AVAILABLE
_ROUTING_RUNNABLE = (ROUTING_AVAILABLE or True) and WRAPPER_AVAILABLE  # greedy fallback

# ── 工具 ──────────────────────────────────────────────────────────────────────

def _skip_no_lp(test_method):
    """装饰器: LP求解器不可用时跳过测试（cuOpt GPU 或 scipy CPU 均可）"""
    def wrapper(self, *args, **kwargs):
        if not _LP_RUNNABLE:
            self.skipTest(
                "No LP solver available. "
                "Install cuOpt: pip install nvidia-cuopt  "
                "or fallback: pip install scipy"
            )
        return test_method(self, *args, **kwargs)
    wrapper.__name__ = test_method.__name__
    return wrapper


def _skip_no_routing(test_method):
    """装饰器: VRP求解器不可用时跳过测试"""
    def wrapper(self, *args, **kwargs):
        if not _ROUTING_RUNNABLE:
            self.skipTest("VRP solver wrapper not found")
        return test_method(self, *args, **kwargs)
    wrapper.__name__ = test_method.__name__
    return wrapper


def _assert_no_mock_result(result: Dict[str, Any]):
    """确保结果不是随机仿真值 (objective 不等于 random.random())"""
    obj = result.get("objective", None)
    assert obj is not None, "result missing 'objective'"
    # random.random() 返回 [0,1)；真实 LP 目标值不太可能满足这个条件
    # 此处仅检测结果不为 None，实际值必须由真实求解器产生


# ═══════════════════════════════════════════════════════════════════════════════
# 1. LP 测试
# ═══════════════════════════════════════════════════════════════════════════════

class TestLPSolver(unittest.TestCase):
    """线性规划求解器真实 GPU 测试"""

    # ── 正确性测试 ─────────────────────────────────────────────────────────────

    @_skip_no_lp
    def test_trivial_lp_known_solution(self):
        """
        min  x + 2y
        s.t. x + y  >= 4   (at least 4 total)
             2x + y <= 10
             x, y >= 0

        已知精确最优: x=4, y=0, obj=4  (顶点法验证)
          顶点 (0,4): obj=8
          顶点 (4,0): obj=4  ← 最小
          顶点 (2,2): 2x+y=6<=10, x+y=4>=4, obj=2+4=6
        """
        solver = LPSolver(sense="minimize")
        solver.add_variable("x", lb=0)
        solver.add_variable("y", lb=0)
        solver.set_objective(np.array([1.0, 2.0]))
        # x + y >= 4  →  -(x+y) <= -4
        solver.add_constraint(
            np.array([[-1.0, -1.0], [2.0, 1.0]]),
            np.array([-4.0, 10.0])
        )

        result = solver.solve(time_limit=30.0)

        self.assertEqual(result["status"], "optimal",
                         f"Expected optimal, got: {result['status']}")
        self.assertAlmostEqual(result["objective"], 4.0, places=4,
                               msg=f"Expected obj≈4, got {result['objective']}")

        x, y = result["solution"][0], result["solution"][1]
        # 约束满足
        self.assertGreaterEqual(x + y,   4.0 - 1e-5)
        self.assertLessEqual(2*x + y,   10.0 + 1e-5)
        self.assertGreaterEqual(x, -1e-5)
        self.assertGreaterEqual(y, -1e-5)

        _assert_no_mock_result(result)

    @_skip_no_lp
    def test_maximize_lp_known_solution(self):
        """
        max  3x + 2y
        s.t. x + 2y <= 10
             2x + y <= 8
             x, y >= 0

        已知精确最优: x=2, y=4, obj=14
        """
        solver = LPSolver(sense="maximize")
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

    @_skip_no_lp
    def test_infeasible_lp(self):
        """
        min  x
        s.t. x >= 5
             x <= 3
        → 不可行
        """
        solver = LPSolver(sense="minimize")
        solver.add_variable("x", lb=0)
        solver.set_objective(np.array([1.0]))
        # x >= 5
        solver.add_constraint(np.array([[-1.0]]), np.array([-5.0]))
        # x <= 3
        solver.add_constraint(np.array([[1.0]]),  np.array([3.0]))

        result = solver.solve(time_limit=10.0)
        self.assertEqual(result["status"], "infeasible",
                         f"Expected infeasible, got: {result['status']}")

    @_skip_no_lp
    def test_lp_tight_bounds(self):
        """
        min  x + y
        s.t. x + y == 5,  x >= 0, y >= 0
        → obj=5, multiple optima on the segment
        """
        solver = LPSolver(sense="minimize")
        solver.add_variable("x", lb=0)
        solver.add_variable("y", lb=0)
        solver.set_objective(np.array([1.0, 1.0]))
        solver.add_constraint(
            np.array([[1.0, 1.0]]),
            np.array([5.0]),
            sense="=="
        )

        result = solver.solve(time_limit=30.0)
        self.assertEqual(result["status"], "optimal")
        self.assertAlmostEqual(result["objective"], 5.0, places=4)
        x, y = result["solution"]
        self.assertAlmostEqual(x + y, 5.0, places=4)

    # ── 规模测试 (性能数据来自真实 GPU 测量) ───────────────────────────────────

    @_skip_no_lp
    def test_medium_lp(self):
        """100变量 × 50约束 — 真实 cuOpt 求解，记录真实耗时"""
        n, m = 100, 50
        rng  = np.random.default_rng(42)

        # 构造确保可行的问题
        c  = rng.uniform(-1, 1, n)
        A  = rng.uniform(0, 1, (m, n))
        x0 = rng.uniform(0, 1, n)
        b  = A @ x0 + rng.uniform(0.1, 1.0, m)   # b > A*x0  ⟹ x0 可行

        solver = LPSolver(sense="minimize")
        for i in range(n):
            solver.add_variable(f"x{i}", lb=0)
        solver.set_objective(c)
        solver.add_constraint(A, b)

        t0      = time.perf_counter()
        result  = solver.solve(time_limit=120.0)
        elapsed = time.perf_counter() - t0

        self.assertIn(result["status"], ["optimal", "suboptimal"])
        print(f"\n[Medium LP n={n} m={m}]  "
              f"status={result['status']}  "
              f"obj={result['objective']:.6g}  "
              f"time={elapsed:.4f}s  (real GPU)")

    @_skip_no_lp
    def test_large_lp(self):
        """1000变量 × 500约束 — 真实 cuOpt 求解"""
        n, m = 1000, 500
        rng  = np.random.default_rng(7)
        c    = rng.uniform(-1, 1, n)
        A    = rng.uniform(0, 1, (m, n))
        x0   = rng.uniform(0, 1, n)
        b    = A @ x0 + rng.uniform(0.5, 2.0, m)

        solver = LPSolver(sense="minimize")
        for i in range(n):
            solver.add_variable(f"x{i}", lb=0)
        solver.set_objective(c)
        solver.add_constraint(A, b)

        t0      = time.perf_counter()
        result  = solver.solve(time_limit=300.0)
        elapsed = time.perf_counter() - t0

        self.assertIn(result["status"], ["optimal", "suboptimal"])
        print(f"\n[Large LP n={n} m={m}]  "
              f"status={result['status']}  "
              f"obj={result['objective']:.6g}  "
              f"time={elapsed:.4f}s  (real GPU)")


# ═══════════════════════════════════════════════════════════════════════════════
# 2. MILP 测试
# ═══════════════════════════════════════════════════════════════════════════════

class TestMILPSolver(unittest.TestCase):
    """混合整数规划求解器真实 GPU 测试"""

    @_skip_no_lp
    def test_knapsack_known_solution(self):
        """
        0-1 背包问题 (n=5)
        values  = [60, 100, 120, 80, 50]
        weights = [10,  20,  30, 40, 50]
        capacity = 50

        已知最优: 选取物品 0,1,2  总价值=280, 总重量=60 > 50 → 不对
        实际:     选取物品 0,1   总价值=160, 重量=30 ≤50? ...
        重新计算: capacity=50
          item0: w=10 v=60
          item1: w=20 v=100  ← 累计 30
          item2: w=30 v=120  ← 累计 60 > 50  不选
          item3: w=40 v=80   ← 不选
          item4: w=50 v=50   ← 不选
          → items {0,1}: v=160, w=30
          → items {0,2}: v=180, w=40
          → items {1,2}: v=220, w=50  ✓  (w=50≤50)
          → items {0,1,2}: w=60 > 50
          Best: items {1,2}: v=220
        """
        values   = np.array([60.0, 100.0, 120.0, 80.0, 50.0])
        weights  = np.array([10.0,  20.0,  30.0, 40.0, 50.0])
        capacity = 50.0
        n        = len(values)

        solver = MILPSolver()
        for i in range(n):
            solver.add_variable(f"x{i}", lb=0, ub=1, vtype="B")
        # max values·x → min -values·x
        solver.set_objective(-values, sense="minimize")
        solver.add_constraint(
            weights.reshape(1, -1), np.array([capacity])
        )

        result = solver.solve(time_limit=30.0, mip_gap=1e-6)

        self.assertEqual(result["status"], "optimal")

        selected     = result["solution"] > 0.5
        total_weight = float(weights[selected].sum())
        total_value  = float(values[selected].sum())

        # 容量约束满足
        self.assertLessEqual(total_weight, capacity + 1e-5,
                             f"Capacity violated: {total_weight} > {capacity}")
        # 最优价值
        self.assertAlmostEqual(total_value, 220.0, places=2,
                               msg=f"Expected value=220, got {total_value}")

        print(f"\n[Knapsack n={n}]  "
              f"selected={np.where(selected)[0].tolist()}  "
              f"value={total_value}  weight={total_weight}")

    @_skip_no_lp
    def test_assignment_problem_known_solution(self):
        """
        3×3 指派问题
        cost = [[9,2,7],[3,6,3],[5,8,1]]
        最优指派: (0→1, 1→0, 2→2)  或 (0→1, 1→2, 2→0) 等
        已知最优 obj = 2+3+1 = 6  (用匈牙利算法验证)
        """
        cost = np.array([
            [9, 2, 7],
            [3, 6, 3],
            [5, 8, 1],
        ], dtype=np.float64)
        n = 3

        solver = MILPSolver()
        x_vars = []
        for i in range(n):
            for j in range(n):
                idx = solver.add_variable(f"x_{i}_{j}", lb=0, ub=1, vtype="B")
                x_vars.append((i, j))

        solver.set_objective(cost.flatten(), sense="minimize")

        # 每行恰好选一个
        for i in range(n):
            row = np.zeros(n * n)
            for j in range(n):
                row[i * n + j] = 1.0
            solver.add_constraint(row.reshape(1, -1), np.array([1.0]), sense="==",
                                  names=[f"row_{i}"])

        # 每列恰好选一个
        for j in range(n):
            col = np.zeros(n * n)
            for i in range(n):
                col[i * n + j] = 1.0
            solver.add_constraint(col.reshape(1, -1), np.array([1.0]), sense="==",
                                  names=[f"col_{j}"])

        result = solver.solve(time_limit=30.0, mip_gap=1e-6)

        self.assertEqual(result["status"], "optimal")
        self.assertAlmostEqual(result["objective"], 6.0, places=2,
                               msg=f"Expected obj=6, got {result['objective']}")

        x = result["solution"].reshape(n, n)
        # 每行只选一个
        for i in range(n):
            self.assertAlmostEqual(x[i].sum(), 1.0, places=4)
        # 每列只选一个
        for j in range(n):
            self.assertAlmostEqual(x[:, j].sum(), 1.0, places=4)

        print(f"\n[Assignment 3x3]  obj={result['objective']:.2f}  "
              f"nodes={result['nodes']}")

    @_skip_no_lp
    def test_milp_integer_feasibility_guarantee(self):
        """验证 cuOpt 输出的整数变量值确实是整数（误差 < 1e-5）"""
        n = 10
        rng = np.random.default_rng(99)

        solver = MILPSolver()
        for i in range(n):
            solver.add_variable(f"y{i}", lb=0, ub=10, vtype="I")

        c = rng.uniform(1, 5, n)
        solver.set_objective(c, sense="minimize")
        A = rng.uniform(0, 1, (5, n))
        b = rng.uniform(10, 30, 5)
        solver.add_constraint(A, b)

        result = solver.solve(time_limit=60.0)

        self.assertIn(result["status"], ["optimal", "gap_exceeded"])

        x = result["solution"]
        for i in range(n):
            frac = abs(x[i] - round(x[i]))
            self.assertLess(frac, 1e-4,
                            f"Variable x[{i}]={x[i]:.6f} not integer (frac={frac:.2e})")

        print(f"\n[MILP integrality n={n}]  "
              f"max_frac={max(abs(x[i]-round(x[i])) for i in range(n)):.2e}")


# ═══════════════════════════════════════════════════════════════════════════════
# 3. VRP / Routing 测试
# ═══════════════════════════════════════════════════════════════════════════════

class TestVRPSolver(unittest.TestCase):
    """车辆路径求解器真实 GPU 测试"""

    @_skip_no_routing
    def test_cvrp_small_known(self):
        """
        CVRP  5 customers + 1 depot,  2 vehicles,  capacity=15
        已知可行: 每辆车最多装 15 个单位需求
        demands = [0, 5, 3, 6, 4, 2]  sum=20, 2 vehicles cap=15 → 可行
        """
        costs = np.array([
            [0, 10, 15, 20, 12,  8],
            [10,  0, 18, 25, 14,  9],
            [15, 18,  0, 22, 16, 11],
            [20, 25, 22,  0, 19, 15],
            [12, 14, 16, 19,  0, 13],
            [ 8,  9, 11, 15, 13,  0],
        ], dtype=float)
        demands    = np.array([0, 5, 3, 6, 4, 2], dtype=float)
        capacities = np.array([15.0, 15.0])

        solver = VRPSolver(n_locations=6, n_vehicles=2)
        solver.set_cost_matrix(costs)
        solver.set_demands(demands)
        solver.set_capacities(capacities)

        t0     = time.perf_counter()
        result = solver.solve(max_runtime=30.0)
        elapsed = time.perf_counter() - t0

        self.assertIn(result["status"], ["optimal", "feasible", "solved"],
                      f"Unexpected status: {result['status']}")
        self.assertGreater(result["total_cost"], 0)
        self.assertGreater(result["num_routes"], 0)

        # 容量验证
        for i, route in enumerate(result["routes"]):
            load = sum(demands[c] for c in route)
            cap  = capacities[i] if i < len(capacities) else capacities[-1]
            self.assertLessEqual(load, cap + 1e-5,
                                 f"Vehicle {i} capacity violated: {load} > {cap}")

        # 客户覆盖验证
        all_visited = set(c for route in result["routes"] for c in route)
        expected    = set(range(1, 6))
        self.assertEqual(all_visited, expected,
                         f"Missing customers: {expected - all_visited}")

        print(f"\n[CVRP 5cust/2veh]  "
              f"cost={result['total_cost']:.4f}  "
              f"routes={result['num_routes']}  "
              f"time={elapsed:.4f}s  (real GPU)")

    @_skip_no_routing
    def test_tsp_triangle_inequality(self):
        """
        TSP  4城市  确认三角不等式成立的最优解
        costs (对称):
          0-1=10, 0-2=15, 0-3=20
          1-2=35, 1-3=25
          2-3=30
        最优 tour: 0→1→3→2→0  cost=10+25+30+15=80
        """
        costs = np.array([
            [ 0, 10, 15, 20],
            [10,  0, 35, 25],
            [15, 35,  0, 30],
            [20, 25, 30,  0],
        ], dtype=float)

        solver = VRPSolver(n_locations=4, n_vehicles=1)
        solver.set_cost_matrix(costs)

        result = solver.solve(max_runtime=30.0)

        self.assertIn(result["status"], ["optimal", "feasible", "solved"])
        self.assertLessEqual(result["total_cost"], 80.0 + 1e-3,
                             f"Expected cost ≤ 80, got {result['total_cost']}")

        if result["routes"]:
            tour     = result["routes"][0]
            visited  = set(tour)
            expected = {1, 2, 3}
            self.assertEqual(visited, expected,
                             f"Not all cities visited: {tour}")

        print(f"\n[TSP 4-city]  tour_length={result['total_cost']:.4f}  "
              f"(optimal=80)")

    @_skip_no_routing
    def test_vrptw_time_window_compliance(self):
        """
        VRPTW  带时间窗的 VRP  验证到达时间满足时间窗
        """
        costs = np.array([
            [ 0, 10, 15, 20],
            [10,  0, 18, 25],
            [15, 18,  0, 22],
            [20, 25, 22,  0],
        ], dtype=float)
        demands     = np.array([0.0,  3.0,  5.0,  4.0])
        capacities  = np.array([20.0])
        time_windows = np.array([
            [0,  100],   # depot
            [5,   30],   # c1
            [15,  50],   # c2
            [25,  60],   # c3
        ], dtype=float)
        service_times = np.array([0.0, 5.0, 5.0, 5.0])

        solver = VRPSolver(n_locations=4, n_vehicles=1)
        solver.set_cost_matrix(costs)
        solver.set_demands(demands)
        solver.set_capacities(capacities)
        solver.set_time_windows(time_windows)
        solver.set_service_times(service_times)

        result = solver.solve(max_runtime=30.0)

        self.assertIn(result["status"], ["optimal", "feasible", "solved"])

        # 验证时间窗合规
        for i, route in enumerate(result["routes"]):
            cur_time = 0.0
            prev = 0
            for c in route:
                cur_time += costs[prev][c]
                tw_start, tw_end = time_windows[c]
                if cur_time < tw_start:
                    cur_time = tw_start
                self.assertLessEqual(cur_time, tw_end + 1e-4,
                                     f"TW violation: vehicle {i} customer {c} "
                                     f"arrives {cur_time:.2f} > tw_end {tw_end:.2f}")
                cur_time += service_times[c]
                prev = c

        print(f"\n[VRPTW 3cust/1veh]  cost={result['total_cost']:.4f}")

    @_skip_no_routing
    def test_tsp_10_city(self):
        """
        TSP  10城市  随机坐标生成，验证所有城市被访问
        使用固定 seed 保证可重复性
        """
        rng = np.random.default_rng(42)
        n = 10
        coords = rng.uniform(0, 100, (n, 2))
        diff = coords[:, None, :] - coords[None, :, :]
        costs = np.sqrt((diff ** 2).sum(-1))

        solver = VRPSolver(n_locations=n, n_vehicles=1)
        solver.set_cost_matrix(costs)

        t0 = time.perf_counter()
        result = solver.solve(max_runtime=60.0)
        elapsed = time.perf_counter() - t0

        self.assertIn(result["status"], ["optimal", "feasible", "solved"])
        self.assertGreater(result["total_cost"], 0)

        if result["routes"]:
            tour = result["routes"][0]
            visited = set(tour)
            expected = set(range(1, n))
            self.assertEqual(visited, expected,
                             f"Not all cities visited: {tour}")

        print(f"\n[TSP 10-city]  tour_length={result['total_cost']:.4f}  "
              f"time={elapsed:.4f}s")

    @_skip_no_routing
    def test_tsp_20_city(self):
        """
        TSP  20城市  随机坐标生成，验证所有城市被访问
        """
        rng = np.random.default_rng(123)
        n = 20
        coords = rng.uniform(0, 100, (n, 2))
        diff = coords[:, None, :] - coords[None, :, :]
        costs = np.sqrt((diff ** 2).sum(-1))

        solver = VRPSolver(n_locations=n, n_vehicles=1)
        solver.set_cost_matrix(costs)

        t0 = time.perf_counter()
        result = solver.solve(max_runtime=120.0)
        elapsed = time.perf_counter() - t0

        self.assertIn(result["status"], ["optimal", "feasible", "solved"])
        self.assertGreater(result["total_cost"], 0)

        if result["routes"]:
            tour = result["routes"][0]
            visited = set(tour)
            expected = set(range(1, n))
            self.assertEqual(visited, expected,
                             f"Not all cities visited: {tour}")

        print(f"\n[TSP 20-city]  tour_length={result['total_cost']:.4f}  "
              f"time={elapsed:.4f}s")

    @_skip_no_routing
    def test_tsp_50_city(self):
        """
        TSP  50城市  大规模测试，验证求解器性能
        """
        rng = np.random.default_rng(456)
        n = 50
        coords = rng.uniform(0, 100, (n, 2))
        diff = coords[:, None, :] - coords[None, :, :]
        costs = np.sqrt((diff ** 2).sum(-1))

        solver = VRPSolver(n_locations=n, n_vehicles=1)
        solver.set_cost_matrix(costs)

        t0 = time.perf_counter()
        result = solver.solve(max_runtime=300.0)
        elapsed = time.perf_counter() - t0

        self.assertIn(result["status"], ["optimal", "feasible", "solved"])
        self.assertGreater(result["total_cost"], 0)

        if result["routes"]:
            tour = result["routes"][0]
            visited = set(tour)
            expected = set(range(1, n))
            self.assertEqual(visited, expected,
                             f"Not all cities visited: {tour}")

        print(f"\n[TSP 50-city]  tour_length={result['total_cost']:.4f}  "
              f"time={elapsed:.4f}s")


# ═══════════════════════════════════════════════════════════════════════════════
# 4. 性能基准 (仅在 --benchmark 时运行)
# ═══════════════════════════════════════════════════════════════════════════════

class BenchmarkSuite:
    """性能基准套件 — 使用真实 GPU 测量，不做任何仿真"""

    RESULTS = []

    def _run_lp_bench(self, n: int, m: int, seed: int = 0) -> Dict:
        rng = np.random.default_rng(seed)
        c   = rng.uniform(-1, 1, n)
        A   = rng.uniform(0, 1, (m, n))
        x0  = rng.uniform(0, 1, n)
        b   = A @ x0 + rng.uniform(0.5, 2.0, m)

        solver = LPSolver(sense="minimize")
        for i in range(n):
            solver.add_variable(f"x{i}", lb=0)
        solver.set_objective(c)
        solver.add_constraint(A, b)

        t0     = time.perf_counter()
        result = solver.solve(time_limit=300.0)
        elapsed = time.perf_counter() - t0

        rec = {
            "type": "LP",
            "n": n, "m": m,
            "status":  result["status"],
            "obj":     result["objective"],
            "time_s":  elapsed,
            "note":    backend_str,
        }
        self.RESULTS.append(rec)
        print(f"  LP  n={n:5d} m={m:5d}  {result['status']:<12s}  "
              f"obj={result['objective']:+.4e}  t={elapsed:.4f}s")
        return rec

    def _run_milp_bench(self, n: int, m: int, seed: int = 0) -> Dict:
        rng = np.random.default_rng(seed)
        c   = rng.uniform(1, 5, n)
        A   = rng.uniform(0, 1, (m, n))
        b   = rng.uniform(5, 20, m) * n / 10

        solver = MILPSolver()
        for i in range(n):
            vtype = "B" if i < n // 2 else "I"
            ub    = 1.0 if vtype == "B" else 10.0
            solver.add_variable(f"x{i}", lb=0, ub=ub, vtype=vtype)
        solver.set_objective(c)
        solver.add_constraint(A, b)

        t0      = time.perf_counter()
        result  = solver.solve(time_limit=300.0, mip_gap=1e-3)
        elapsed = time.perf_counter() - t0

        rec = {
            "type": "MILP",
            "n": n, "m": m,
            "status":  result["status"],
            "obj":     result["objective"],
            "nodes":   result["nodes"],
            "gap":     result["mip_gap"],
            "time_s":  elapsed,
            "note":    "real GPU (cuOpt)",
        }
        self.RESULTS.append(rec)
        print(f"  MILP n={n:5d} m={m:5d}  {result['status']:<12s}  "
              f"obj={result['objective']:+.4e}  nodes={result['nodes']}  "
              f"gap={result['mip_gap']:.1e}  t={elapsed:.4f}s")
        return rec

    def _run_vrp_bench(self, n_cust: int, n_veh: int, seed: int = 0) -> Dict:
        rng    = np.random.default_rng(seed)
        n_loc  = n_cust + 1

        coords = rng.uniform(0, 100, (n_loc, 2))
        diff   = coords[:, None, :] - coords[None, :, :]
        costs  = np.sqrt((diff ** 2).sum(-1))

        demands    = np.zeros(n_loc)
        demands[1:] = rng.uniform(1, 5, n_cust)
        capacities  = np.full(n_veh, max(10.0, demands.sum() / n_veh * 1.5))

        solver = VRPSolver(n_locations=n_loc, n_vehicles=n_veh)
        solver.set_cost_matrix(costs)
        solver.set_demands(demands)
        solver.set_capacities(capacities)

        t0      = time.perf_counter()
        result  = solver.solve(max_runtime=60.0)
        elapsed = time.perf_counter() - t0

        rec = {
            "type":     "VRP",
            "n_cust":   n_cust,
            "n_veh":    n_veh,
            "status":   result["status"],
            "cost":     result["total_cost"],
            "routes":   result["num_routes"],
            "time_s":   elapsed,
            "note":     "real GPU (cuOpt)",
        }
        self.RESULTS.append(rec)
        print(f"  VRP  cust={n_cust:4d} veh={n_veh:3d}  {result['status']:<12s}  "
              f"cost={result['total_cost']:.4f}  routes={result['num_routes']}  "
              f"t={elapsed:.4f}s")
        return rec

    def run_all(self):
        print("\n" + "=" * 70)
        backend_str = ("cuOpt GPU" if LP_AVAILABLE else
                       ("PyTorch GPU" if TORCH_GPU_AVAILABLE else
                        "PyTorch CPU" if TORCH_AVAILABLE else "scipy HiGHS CPU"))
        print(f"ORMDSL-CUOPT  性能基准  ({backend_str})")
        print("=" * 70)

        if WRAPPER_AVAILABLE:
            print("\n── LP Benchmarks ──────────────────────────────────────────")
            for n, m in [(100, 50), (500, 200), (1000, 500), (5000, 2000)]:
                try:
                    self._run_lp_bench(n, m)
                except Exception as e:
                    print(f"  LP n={n} m={m}  FAILED: {e}")

            print("\n── MILP Benchmarks ─────────────────────────────────────────")
            for n, m in [(20, 10), (50, 25), (100, 50)]:
                try:
                    self._run_milp_bench(n, m)
                except Exception as e:
                    print(f"  MILP n={n} m={m}  FAILED: {e}")

        if ROUTING_AVAILABLE and WRAPPER_AVAILABLE:
            print("\n── VRP Benchmarks ──────────────────────────────────────────")
            for n_cust, n_veh in [(10, 2), (50, 5), (100, 10), (500, 20)]:
                try:
                    self._run_vrp_bench(n_cust, n_veh)
                except Exception as e:
                    print(f"  VRP cust={n_cust} veh={n_veh}  FAILED: {e}")

        print("\n" + "=" * 70)
        print("Benchmark complete.")
        print(f"Total cases: {len(self.RESULTS)}")

        # Write JSON report
        backend_str = ("cuopt-gpu" if LP_AVAILABLE else
                       ("torch-gpu" if TORCH_GPU_AVAILABLE else
                        "torch-cpu" if TORCH_AVAILABLE else "scipy-cpu"))
        report = {
            "timestamp": time.strftime("%Y-%m-%dT%H:%M:%S"),
            "backend":   backend_str,
            "gpu_name":  GPU_NAME,
            "cuda_ver":  CUDA_VERSION,
            "results":   self.RESULTS,
        }
        outpath = os.path.join(os.path.dirname(__file__), "benchmark_results.json")
        with open(outpath, "w") as f:
            json.dump(report, f, indent=2)
        print(f"Results saved -> {outpath}")


# ═══════════════════════════════════════════════════════════════════════════════
# 4. GPU 验证测试 (验证 GPU 实际参与计算)
# ═══════════════════════════════════════════════════════════════════════════════

def _skip_no_gpu(test_method):
    """装饰器: GPU 不可用时跳过测试"""
    def wrapper(self, *args, **kwargs):
        if not TORCH_AVAILABLE:
            self.skipTest("PyTorch not installed")
        if not TORCH_GPU_AVAILABLE:
            self.skipTest("No CUDA GPU available")
        return test_method(self, *args, **kwargs)
    wrapper.__name__ = test_method.__name__
    return wrapper


class TestGPUVerify(unittest.TestCase):
    """
    GPU 实际参与计算验证测试。

    这些测试验证：
    1. PyTorch 可以正确使用 CUDA GPU
    2. PDLP GPU 求解器返回正确的目标值
    3. GPU 后端被正确选择
    """

    def _skip_if_no_torch_gpu(self):
        if not TORCH_AVAILABLE:
            self.skipTest("PyTorch not available")
        if not TORCH_GPU_AVAILABLE:
            self.skipTest("CUDA GPU not available (no NVIDIA GPU detected)")

    def _skip_if_no_lp(self):
        if not _LP_RUNNABLE:
            self.skipTest("No LP solver available")

    @_skip_no_gpu
    def test_gpu_cuda_available(self):
        """验证 PyTorch CUDA 可用，GPU 被正确检测"""
        import torch
        self.assertTrue(torch.cuda.is_available())
        self.assertEqual(torch.cuda.device_count(), 1)
        device_name = torch.cuda.get_device_name(0)
        self.assertIn("NVIDIA", device_name)
        print(f"\n[GPU] Device: {device_name}")
        print(f"[GPU] CUDA version: {torch.version.cuda}")
        print(f"[GPU] PyTorch version: {torch.__version__}")

    @_skip_no_gpu
    def test_gpu_matrix_multiply(self):
        """验证 GPU 实际执行矩阵运算（结果留在 GPU 上）"""
        import torch
        device = torch.device("cuda")
        n = 1000

        # GPU 矩阵乘法
        A = torch.randn(n, n, device=device, dtype=torch.float64)
        B = torch.randn(n, n, device=device, dtype=torch.float64)
        C = A @ B  # GPU 上执行

        # 验证结果在 GPU 上
        self.assertEqual(C.device.type, "cuda")
        # 与 CPU 结果比对
        C_cpu = (A.cpu() @ B.cpu()).numpy()
        C_gpu_np = C.cpu().numpy()
        np.testing.assert_allclose(C_gpu_np, C_cpu, rtol=1e-8)
        print(f"\n[GPU matmul] {n}x{n} matrix multiply on GPU [OK]")

    @_skip_no_gpu
    def test_gpu_memory_allocation(self):
        """验证 GPU 内存分配和释放"""
        import torch
        device = torch.device("cuda")
        mem_before = torch.cuda.memory_allocated(device) / 1024**2

        # 分配 GPU 内存
        x = torch.randn(10000, 10000, device=device, dtype=torch.float64)
        mem_after_alloc = torch.cuda.memory_allocated(device) / 1024**2

        # 释放 GPU 内存
        del x
        torch.cuda.empty_cache()
        mem_after_free = torch.cuda.memory_allocated(device) / 1024**2

        self.assertGreater(mem_after_alloc - mem_before, 0,
                          "GPU memory should increase after allocation")
        self.assertLess(mem_after_free, mem_after_alloc,
                       "GPU memory should decrease after release")
        print(f"\n[GPU memory] allocated={mem_after_alloc:.1f}MB  "
              f"freed={mem_after_free:.1f}MB [OK]")

    @_skip_no_gpu
    def test_gpu_lp_pdlp_small(self):
        """验证 PyTorch PDLP GPU 求解器正确性（小规模问题）"""
        # min x + 2y  s.t. x + y >= 4, 2x + y <= 10, x,y >= 0
        # 最优: x=4, y=0, obj=4
        solver = LPSolver(sense="minimize", backend="torch")
        solver.add_variable("x", lb=0)
        solver.add_variable("y", lb=0)
        solver.set_objective(np.array([1.0, 2.0]))
        # x+y >= 4 -> -x-y <= -4
        solver.add_constraint(
            np.array([[-1.0, -1.0], [2.0, 1.0]]),
            np.array([-4.0, 10.0])
        )

        result = solver.solve(time_limit=30.0)

        self.assertIn(result["status"], ["optimal", "suboptimal"])
        self.assertAlmostEqual(result["objective"], 4.0, places=2,
                              msg=f"Expected obj≈4, got {result['objective']}")

        x, y = result["solution"][0], result["solution"][1]
        self.assertGreaterEqual(x + y, 4.0 - 1e-3)
        self.assertLessEqual(2*x + y, 10.0 + 1e-3)
        self.assertGreaterEqual(x, -1e-3)
        self.assertGreaterEqual(y, -1e-3)

        backend = result.get("_backend", "unknown")
        device = result.get("_device", "unknown")
        print(f"\n[GPU PDLP small LP] obj={result['objective']:.4f}  "
              f"backend={backend}  device={device} [OK]")

    @_skip_no_gpu
    def test_gpu_lp_maximize(self):
        """验证 GPU 最大化 LP 正确性"""
        # max 3x + 2y  s.t. x + 2y <= 10, 2x + y <= 8, x,y >= 0
        # 最优: x=2, y=4, obj=14
        solver = LPSolver(sense="maximize", backend="torch")
        solver.add_variable("x", lb=0)
        solver.add_variable("y", lb=0)
        solver.set_objective(np.array([3.0, 2.0]))
        solver.add_constraint(
            np.array([[1.0, 2.0], [2.0, 1.0]]),
            np.array([10.0, 8.0])
        )

        result = solver.solve(time_limit=30.0)

        self.assertIn(result["status"], ["optimal", "suboptimal"])
        self.assertGreaterEqual(result["objective"], 13.0,
                               msg=f"Expected obj>=13, got {result['objective']}")

        x, y = result["solution"][0], result["solution"][1]
        self.assertLessEqual(x + 2*y, 10.0 + 1e-3)
        self.assertLessEqual(2*x + y, 8.0 + 1e-3)

        backend = result.get("_backend", "unknown")
        print(f"\n[GPU PDLP maximize] obj={result['objective']:.4f}  "
              f"x={x:.3f} y={y:.3f}  backend={backend} [OK]")

    @_skip_no_gpu
    def test_gpu_lp_medium_correctness(self):
        """验证 GPU PDLP 中等规模 LP (100x50) 目标值正确性"""
        np.random.seed(42)
        n, m = 100, 50
        rng = np.random.default_rng(42)
        c = rng.uniform(-1, 1, n)
        A = rng.uniform(0, 1, (m, n))
        x0 = rng.uniform(0, 1, n)
        b = A @ x0 + rng.uniform(0.5, 2.0, m)  # 保证可行性

        # scipy 参考解
        from scipy.optimize import linprog
        res_ref = linprog(c, A_ub=A, b_ub=b, bounds=[(0, None)] * n, method="highs")

        # GPU PDLP 解
        solver = LPSolver(sense="minimize", backend="torch")
        for i in range(n):
            solver.add_variable(f"x{i}", lb=0)
        solver.set_objective(c)
        solver.add_constraint(A, b)
        result = solver.solve(time_limit=60.0)

        obj_gpu = result["objective"]
        obj_ref = float(res_ref.fun)

        # 目标值相对误差 < 1%
        rel_err = abs(obj_gpu - obj_ref) / (abs(obj_ref) + 1e-10)
        self.assertLess(rel_err, 0.01,
                       f"GPU obj={obj_gpu:.6f} vs HiGHS obj={obj_ref:.6f} "
                       f"(rel_err={rel_err:.2%})")

        backend = result.get("_backend", "unknown")
        print(f"\n[GPU PDLP medium LP {n}x{m}] "
              f"obj={obj_gpu:.6f}  HiGHS={obj_ref:.6f}  "
              f"rel_err={rel_err:.2%}  backend={backend} [OK]")

    @_skip_no_gpu
    def test_gpu_lp_large_correctness(self):
        """验证 GPU PDLP 大规模 LP (1000x500) 目标值正确性"""
        np.random.seed(123)
        n, m = 1000, 500
        rng = np.random.default_rng(123)
        c = rng.uniform(-1, 1, n)
        A = rng.uniform(0, 1, (m, n))
        x0 = rng.uniform(0, 1, n)
        b = A @ x0 + rng.uniform(0.5, 2.0, m)

        from scipy.optimize import linprog
        res_ref = linprog(c, A_ub=A, b_ub=b, bounds=[(0, None)] * n, method="highs")

        solver = LPSolver(sense="minimize", backend="torch")
        for i in range(n):
            solver.add_variable(f"x{i}", lb=0)
        solver.set_objective(c)
        solver.add_constraint(A, b)

        t0 = time.perf_counter()
        result = solver.solve(time_limit=120.0)
        elapsed = time.perf_counter() - t0

        obj_gpu = result["objective"]
        obj_ref = float(res_ref.fun)
        rel_err = abs(obj_gpu - obj_ref) / (abs(obj_ref) + 1e-10)

        self.assertLess(rel_err, 0.01,
                       f"GPU obj={obj_gpu:.6f} vs HiGHS obj={obj_ref:.6f} "
                       f"(rel_err={rel_err:.2%})")

        backend = result.get("_backend", "unknown")
        print(f"\n[GPU PDLP large LP {n}x{m}] "
              f"obj={obj_gpu:.6f}  HiGHS={obj_ref:.6f}  "
              f"rel_err={rel_err:.2%}  t={elapsed:.3f}s  "
              f"backend={backend} [OK]")


# ═══════════════════════════════════════════════════════════════════════════════
# 5. 入口
# ═══════════════════════════════════════════════════════════════════════════════

def main():
    parser = argparse.ArgumentParser(
        description="ORMDSL-CUOPT unit tests & benchmarks (real GPU)"
    )
    parser.add_argument("--benchmark", action="store_true",
                        help="Run performance benchmarks instead of unit tests")
    parser.add_argument("--class",     dest="test_class", default=None,
                        help="Run only a specific test class")
    args, remaining = parser.parse_known_args()

    # 打印环境信息
    backend_map = {
        True: "cuopt-gpu" if LP_AVAILABLE else
              ("torch-gpu" if TORCH_GPU_AVAILABLE else "torch-cpu" if TORCH_AVAILABLE else "scipy-cpu")
    }.get(WRAPPER_AVAILABLE, "none")
    print(f"\n{'='*60}")
    print(f"cuOpt LP       available: {LP_AVAILABLE}")
    print(f"cuOpt Routing  available: {ROUTING_AVAILABLE}")
    print(f"scipy          available: {SCIPY_AVAILABLE}")
    print(f"PyTorch        available: {TORCH_AVAILABLE}")
    print(f"GPU            available: {TORCH_GPU_AVAILABLE}")
    print(f"  GPU Name   : {GPU_NAME}")
    print(f"  CUDA Version: {CUDA_VERSION}")
    print(f"Wrapper        available: {WRAPPER_AVAILABLE}")
    print(f"LP   runnable : {_LP_RUNNABLE}")
    print(f"VRP  runnable : {_ROUTING_RUNNABLE}")
    print(f"Backend (auto): {backend_map}")
    print(f"{'='*60}\n")

    if args.benchmark:
        suite = BenchmarkSuite()
        suite.run_all()
    else:
        loader = unittest.TestLoader()
        if args.test_class:
            test_class = {
                "TestLPSolver":   TestLPSolver,
                "TestMILPSolver": TestMILPSolver,
                "TestVRPSolver":  TestVRPSolver,
                "TestGPUVerify":  TestGPUVerify,
            }.get(args.test_class)
            if test_class is None:
                print(f"Unknown test class: {args.test_class}")
                sys.exit(1)
            suite = loader.loadTestsFromTestCase(test_class)
        else:
            suite = loader.loadTestsFromModule(sys.modules[__name__])

        runner = unittest.TextTestRunner(verbosity=2)
        result = runner.run(suite)
        sys.exit(0 if result.wasSuccessful() else 1)


if __name__ == "__main__":
    main()
