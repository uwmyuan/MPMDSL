package ormcuopt

/**
 * Python API包装器生成器
 * 生成使用真实 NVIDIA cuOpt GPU求解器的 Python 接口代码
 *
 * 依赖: pip install nvidia-cuopt
 * 文档: https://docs.nvidia.com/cuopt/user-guide/latest/
 */
object PythonAPIGenerator {

  /** 生成 cuopt_solver.py 主模块 */
  def generatePythonWrapper(): String =
    """#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
ORMDSL-CUOPT Python API
GPU-accelerated optimization solver — backed by NVIDIA cuOpt

Install backend:
    pip install nvidia-cuopt

Supported problem types:
    - LP   : Linear Programming        (cuopt LP solver)
    - MILP : Mixed-Integer LP          (cuopt MILP solver)
    - VRP  : Vehicle Routing Problem   (cuopt Routing solver)
"""

import numpy as np
from typing import Dict, Any, List, Optional, Union

# ── cuOpt availability check ────────────────────────────────────────────────
try:
    from cuopt.linear_programming import problem as lp_module
    from cuopt.linear_programming import solver_settings as ss_module
    _LP_AVAILABLE = True
except ImportError:
    _LP_AVAILABLE = False

try:
    from cuopt.routing import routing as routing_module
    _ROUTING_AVAILABLE = True
except ImportError:
    _ROUTING_AVAILABLE = False

_CUOPT_AVAILABLE = _LP_AVAILABLE or _ROUTING_AVAILABLE


def _require_lp():
    if not _LP_AVAILABLE:
        raise ImportError(
            "cuOpt LP/MILP solver not found.\n"
            "Install with:  pip install nvidia-cuopt"
        )


def _require_routing():
    if not _ROUTING_AVAILABLE:
        raise ImportError(
            "cuOpt Routing solver not found.\n"
            "Install with:  pip install nvidia-cuopt"
        )


# ── LP Solver ────────────────────────────────────────────────────────────────

class LPSolver:
    """
    Linear Programming solver using NVIDIA cuOpt.

    Usage::

        solver = LPSolver(sense="minimize")
        solver.add_variable("x", lb=0)
        solver.add_variable("y", lb=0)
        solver.set_objective(np.array([1.0, 2.0]))
        solver.add_constraint(
            np.array([[1.0, 1.0], [2.0, 1.0]]),
            np.array([4.0, 5.0]),
            sense="<="
        )
        result = solver.solve(time_limit=60.0)
        print(result["objective"], result["solution"])
    """

    def __init__(self, sense: str = "minimize"):
        _require_lp()
        self._prob = lp_module.Problem()
        self._vars: List[Any] = []           # cuOpt variable objects
        self._var_names: List[str] = []
        self._sense = sense.strip().lower()

    # ── variable / objective / constraint ────────────────────────────

    def add_variable(
        self,
        name: str,
        lb: float = 0.0,
        ub: float = float("inf"),
        vtype: str = "C",
    ) -> int:
        """Add a decision variable.

        Args:
            name:  Variable name (must be unique).
            lb:    Lower bound  (default 0).
            ub:    Upper bound  (default +inf).
            vtype: 'C' continuous, 'I' integer, 'B' binary.

        Returns:
            Zero-based index of the new variable.
        """
        ub_val = lp_module.INF if np.isinf(ub) else float(ub)
        v = self._prob.add_variable(name, float(lb), ub_val, vtype)
        self._vars.append(v)
        self._var_names.append(name)
        return len(self._vars) - 1

    def set_objective(
        self,
        coefficients: np.ndarray,
        sense: Optional[str] = None,
    ) -> None:
        """Set the linear objective  c · x.

        Args:
            coefficients: 1-D array of length n_vars.
            sense:        'minimize' or 'maximize' (overrides constructor value).
        """
        c = np.asarray(coefficients, dtype=np.float64)
        if len(c) != len(self._vars):
            raise ValueError(
                f"len(coefficients)={len(c)} != n_vars={len(self._vars)}"
            )

        s = (sense or self._sense).strip().lower()
        obj_expr = sum(float(c[i]) * self._vars[i] for i in range(len(self._vars)))
        obj_sense = (
            lp_module.MAXIMIZE if s == "maximize" else lp_module.MINIMIZE
        )
        self._prob.set_objective(obj_expr, sense=obj_sense)

    def add_constraint(
        self,
        A: np.ndarray,
        b: np.ndarray,
        sense: str = "<=",
        names: Optional[List[str]] = None,
    ) -> None:
        """Add one or more constraints  A·x  {sense}  b.

        Args:
            A:      2-D matrix (m × n) or 1-D row vector (n,).
            b:      RHS vector of length m (or scalar).
            sense:  '<=', '>=' or '=='.
            names:  Optional list of constraint names.
        """
        A = np.atleast_2d(np.asarray(A, dtype=np.float64))
        b = np.atleast_1d(np.asarray(b, dtype=np.float64))
        m = len(b)

        _sense_map = {
            "<=": lp_module.LESS_EQUALS,
            ">=": lp_module.GREATER_EQUALS,
            "==": lp_module.EQUALS,
        }
        csense = _sense_map.get(sense)
        if csense is None:
            raise ValueError(f"Unknown constraint sense '{sense}'. Use '<=', '>=' or '=='.")

        for i in range(m):
            row = A[i] if A.shape[0] > 1 else A[0]
            expr = sum(float(row[j]) * self._vars[j] for j in range(len(self._vars)))
            cname = names[i] if names and i < len(names) else f"c_{i}"
            self._prob.add_constraint(expr, csense, cname)

    # ── solve ──────────────────────────────────────────────────────────

    def solve(self, time_limit: float = 3600.0) -> Dict[str, Any]:
        """Solve the LP and return a result dict.

        Returns::

            {
                "status":     str,        # 'optimal' | 'suboptimal' | 'infeasible' | 'unbounded'
                "objective":  float,
                "solution":   np.ndarray, # shape (n_vars,)
                "iterations": int,
                "variables":  Dict[str, float]
            }
        """
        settings = ss_module.SolverSettings()
        settings.time_limit = time_limit

        sol = self._prob.solve(settings=settings)

        _status_map = {
            lp_module.OPTIMAL:    "optimal",
            lp_module.SUBOPTIMAL: "suboptimal",
            lp_module.INFEASIBLE: "infeasible",
            lp_module.UNBOUNDED:  "unbounded",
        }
        status = _status_map.get(sol.status, "unknown")

        x = np.array(
            [sol.get_value(v) for v in self._vars],
            dtype=np.float64,
        )

        return {
            "status":     status,
            "objective":  float(sol.objective_value),
            "solution":   x,
            "iterations": getattr(sol, "iterations", 0),
            "variables":  dict(zip(self._var_names, x.tolist())),
        }

    def __repr__(self) -> str:
        return f"LPSolver(n_vars={len(self._vars)}, sense={self._sense!r})"


# ── MILP Solver ──────────────────────────────────────────────────────────────

class MILPSolver:
    """
    Mixed-Integer Linear Programming solver using NVIDIA cuOpt.

    Usage::

        solver = MILPSolver()
        solver.add_variable("x", vtype="B")  # binary
        solver.add_variable("y", vtype="I")  # integer
        solver.set_objective(np.array([3.0, 2.0]))
        solver.add_constraint(np.array([[1.0, 1.0]]), np.array([10.0]))
        result = solver.solve(time_limit=300.0, mip_gap=1e-4)
    """

    def __init__(self):
        _require_lp()
        self._prob = lp_module.Problem()
        self._vars: List[Any] = []
        self._var_names: List[str] = []

    def add_variable(
        self,
        name: str,
        lb: float = 0.0,
        ub: float = float("inf"),
        vtype: str = "C",
    ) -> int:
        """Add a variable.  vtype: 'C' | 'I' | 'B'"""
        ub_val = lp_module.INF if np.isinf(ub) else float(ub)
        if vtype == "B":
            lb, ub_val = 0.0, 1.0
        v = self._prob.add_variable(name, float(lb), ub_val, vtype)
        self._vars.append(v)
        self._var_names.append(name)
        return len(self._vars) - 1

    def set_objective(
        self,
        coefficients: np.ndarray,
        sense: str = "minimize",
    ) -> None:
        """Set objective  c · x."""
        c = np.asarray(coefficients, dtype=np.float64)
        if len(c) != len(self._vars):
            raise ValueError(
                f"len(coefficients)={len(c)} != n_vars={len(self._vars)}"
            )
        obj_expr = sum(float(c[i]) * self._vars[i] for i in range(len(self._vars)))
        obj_sense = (
            lp_module.MAXIMIZE
            if sense.strip().lower() == "maximize"
            else lp_module.MINIMIZE
        )
        self._prob.set_objective(obj_expr, sense=obj_sense)

    def add_constraint(
        self,
        A: np.ndarray,
        b: np.ndarray,
        sense: str = "<=",
        names: Optional[List[str]] = None,
    ) -> None:
        """Add constraints  A·x  {sense}  b."""
        A = np.atleast_2d(np.asarray(A, dtype=np.float64))
        b = np.atleast_1d(np.asarray(b, dtype=np.float64))
        _sense_map = {
            "<=": lp_module.LESS_EQUALS,
            ">=": lp_module.GREATER_EQUALS,
            "==": lp_module.EQUALS,
        }
        csense = _sense_map.get(sense)
        if csense is None:
            raise ValueError(f"Unknown sense '{sense}'.")

        for i in range(len(b)):
            row = A[i] if A.shape[0] > 1 else A[0]
            expr = sum(float(row[j]) * self._vars[j] for j in range(len(self._vars)))
            cname = names[i] if names and i < len(names) else f"c_{i}"
            self._prob.add_constraint(expr, csense, cname)

    def solve(
        self,
        time_limit: float = 3600.0,
        mip_gap: float = 1e-4,
    ) -> Dict[str, Any]:
        """Solve the MILP.

        Returns::

            {
                "status":    str,
                "objective": float,
                "solution":  np.ndarray,
                "nodes":     int,
                "mip_gap":   float,
                "variables": Dict[str, float]
            }
        """
        settings = ss_module.SolverSettings()
        settings.time_limit = time_limit
        if hasattr(settings, "mip_gap"):
            settings.mip_gap = mip_gap

        sol = self._prob.solve(settings=settings)

        _status_map = {
            lp_module.OPTIMAL:    "optimal",
            lp_module.SUBOPTIMAL: "gap_exceeded",
            lp_module.INFEASIBLE: "infeasible",
            lp_module.UNBOUNDED:  "unbounded",
        }
        status = _status_map.get(sol.status, "unknown")

        x = np.array(
            [sol.get_value(v) for v in self._vars],
            dtype=np.float64,
        )

        return {
            "status":    status,
            "objective": float(sol.objective_value),
            "solution":  x,
            "nodes":     int(getattr(sol, "num_nodes", 0)),
            "mip_gap":   float(getattr(sol, "mip_gap", mip_gap)),
            "variables": dict(zip(self._var_names, x.tolist())),
        }

    def __repr__(self) -> str:
        return f"MILPSolver(n_vars={len(self._vars)})"


# ── VRP Solver ───────────────────────────────────────────────────────────────

class VRPSolver:
    """
    Vehicle Routing Problem solver using NVIDIA cuOpt Routing.

    Node 0 is the depot; customers are nodes 1 … n.

    Usage::

        solver = VRPSolver(n_locations=6, n_vehicles=2)
        solver.set_cost_matrix(cost_matrix)          # (6,6) array
        solver.set_demands(demands)                   # (6,)  array, depot=0
        solver.set_capacities(np.array([15.0, 15.0]))
        result = solver.solve(max_runtime=30.0)
        print(result["total_cost"])
        for route in result["routes"]:
            print(route)
    """

    def __init__(self, n_locations: int, n_vehicles: int):
        _require_routing()
        self.n_locations = n_locations
        self.n_vehicles = n_vehicles

        self._data = routing_module.RoutingData(
            n_locations=n_locations,
            n_vehicles=n_vehicles,
        )

    def set_cost_matrix(self, costs: np.ndarray) -> None:
        """Set travel cost matrix shape (n_locations, n_locations)."""
        costs = np.asarray(costs, dtype=np.float64)
        if costs.shape != (self.n_locations, self.n_locations):
            raise ValueError(
                f"cost_matrix shape must be ({self.n_locations},{self.n_locations}), "
                f"got {costs.shape}"
            )
        self._data.set_cost_matrix(costs)

    def set_demands(self, demands: np.ndarray) -> None:
        """Set node demands (depot demand must be 0)."""
        demands = np.asarray(demands, dtype=np.float64)
        self._data.set_demands(demands)

    def set_capacities(self, capacities: np.ndarray) -> None:
        """Set vehicle capacities, shape (n_vehicles,)."""
        capacities = np.asarray(capacities, dtype=np.float64)
        self._data.set_vehicle_capacities(capacities)

    def set_time_windows(self, windows: np.ndarray) -> None:
        """Set time windows shape (n_locations, 2) — [earliest, latest]."""
        windows = np.asarray(windows, dtype=np.float64)
        self._data.set_time_windows(windows)

    def set_service_times(self, service_times: np.ndarray) -> None:
        """Set per-location service time, shape (n_locations,)."""
        service_times = np.asarray(service_times, dtype=np.float64)
        self._data.set_service_times(service_times)

    def solve(
        self,
        max_runtime: float = 30.0,
        n_climbers: Optional[int] = None,
    ) -> Dict[str, Any]:
        """Solve the VRP.

        Args:
            max_runtime: Maximum wall-clock seconds allowed.
            n_climbers:  Number of parallel solution climbers (GPU threads).

        Returns::

            {
                "status":      str,
                "total_cost":  float,
                "num_routes":  int,
                "routes":      List[List[int]],   # customer sequences
                "route_costs": List[float]
            }
        """
        config = routing_module.SolverConfig()
        config.max_runtime = max_runtime
        if n_climbers is not None:
            config.number_of_climbers = n_climbers

        sol = self._data.solve(config)

        routes: List[List[int]] = []
        route_costs: List[float] = []

        for v in range(self.n_vehicles):
            route = sol.get_route(v)           # list of location indices
            cost  = sol.get_route_cost(v)
            if route:                          # skip empty routes
                routes.append(list(route))
                route_costs.append(float(cost))

        return {
            "status":      getattr(sol, "status", "solved"),
            "total_cost":  float(sol.total_cost),
            "num_routes":  len(routes),
            "routes":      routes,
            "route_costs": route_costs,
        }

    def __repr__(self) -> str:
        return (
            f"VRPSolver(n_locations={self.n_locations}, "
            f"n_vehicles={self.n_vehicles})"
        )


# ── Convenience functions ─────────────────────────────────────────────────────

def solve_lp(
    c: np.ndarray,
    A: np.ndarray,
    b: np.ndarray,
    sense: str = "minimize",
    var_lb: Optional[np.ndarray] = None,
    var_ub: Optional[np.ndarray] = None,
    time_limit: float = 3600.0,
) -> Dict[str, Any]:
    """Solve  min/max c·x  s.t. A·x <= b,  lb <= x <= ub.

    Example::

        c = np.array([1.0, 2.0])
        A = np.array([[1.0, 1.0], [2.0, 1.0]])
        b = np.array([4.0, 5.0])
        result = solve_lp(c, A, b)
        print(f"Optimal: {result['objective']:.4f}")
    """
    c  = np.asarray(c, dtype=np.float64)
    n  = len(c)
    lb = np.zeros(n)  if var_lb is None else np.asarray(var_lb,  dtype=np.float64)
    ub = np.full(n, np.inf) if var_ub is None else np.asarray(var_ub, dtype=np.float64)

    solver = LPSolver(sense=sense)
    for i in range(n):
        solver.add_variable(f"x_{i}", lb=lb[i], ub=ub[i])
    solver.set_objective(c)
    solver.add_constraint(A, b)
    return solver.solve(time_limit=time_limit)


def solve_milp(
    c: np.ndarray,
    A: np.ndarray,
    b: np.ndarray,
    vtypes: Union[str, List[str]] = "C",
    sense: str = "minimize",
    var_lb: Optional[np.ndarray] = None,
    var_ub: Optional[np.ndarray] = None,
    time_limit: float = 3600.0,
    mip_gap: float = 1e-4,
) -> Dict[str, Any]:
    """Solve MILP.

    Args:
        vtypes: Single char ('C','I','B') applied to all variables,
                or a list of chars, one per variable.

    Example::

        c      = np.array([3.0, 2.0])
        A      = np.array([[1.0, 1.0]])
        b      = np.array([10.0])
        result = solve_milp(c, A, b, vtypes="II")
        print(f"Optimal: {result['objective']}")
    """
    c  = np.asarray(c, dtype=np.float64)
    n  = len(c)
    lb = np.zeros(n)  if var_lb is None else np.asarray(var_lb,  dtype=np.float64)
    ub = np.full(n, np.inf) if var_ub is None else np.asarray(var_ub, dtype=np.float64)

    # Normalise vtypes to a list of length n
    if isinstance(vtypes, str) and len(vtypes) == 1:
        vt_list: List[str] = [vtypes] * n
    elif isinstance(vtypes, str):
        vt_list = list(vtypes)
    else:
        vt_list = list(vtypes)

    if len(vt_list) != n:
        raise ValueError(f"len(vtypes)={len(vt_list)} != n_vars={n}")

    solver = MILPSolver()
    for i in range(n):
        solver.add_variable(f"x_{i}", lb=lb[i], ub=ub[i], vtype=vt_list[i])
    solver.set_objective(c, sense=sense)
    solver.add_constraint(A, b)
    return solver.solve(time_limit=time_limit, mip_gap=mip_gap)


def solve_vrp(
    cost_matrix: np.ndarray,
    n_vehicles: int,
    demands: Optional[np.ndarray] = None,
    capacities: Optional[np.ndarray] = None,
    time_windows: Optional[np.ndarray] = None,
    max_runtime: float = 30.0,
) -> Dict[str, Any]:
    """Solve a Vehicle Routing Problem.

    Example::

        costs = np.array([[0,10,15],[10,0,20],[15,20,0]], dtype=float)
        result = solve_vrp(costs, n_vehicles=2,
                           demands=np.array([0,3,5]),
                           capacities=np.array([10.0,10.0]))
        print(result["routes"])
    """
    n = len(cost_matrix)
    solver = VRPSolver(n_locations=n, n_vehicles=n_vehicles)
    solver.set_cost_matrix(cost_matrix)

    if demands is not None:
        solver.set_demands(demands)
    if capacities is not None:
        solver.set_capacities(capacities)
    if time_windows is not None:
        solver.set_time_windows(time_windows)

    return solver.solve(max_runtime=max_runtime)
"""

  /** 生成快速入门示例 */
  def generateQuickStart(): String =
    """# ORMDSL-CUOPT Quick-Start Guide

## 1. 安装

```bash
pip install nvidia-cuopt
```

---

## 2. 线性规划 (LP)

```python
from cuopt_solver import LPSolver, solve_lp
import numpy as np

# 方式一：便捷函数
c = np.array([1.0, 2.0])
A = np.array([[1.0, 1.0], [2.0, 1.0]])
b = np.array([4.0, 5.0])

result = solve_lp(c, A, b, sense="minimize")
print(f"Status:    {result['status']}")
print(f"Objective: {result['objective']:.4f}")
print(f"x = {result['solution'][0]:.4f}, y = {result['solution'][1]:.4f}")

# 方式二：面向对象
solver = LPSolver(sense="minimize")
solver.add_variable("x", lb=0)
solver.add_variable("y", lb=0)
solver.set_objective(np.array([1.0, 2.0]))
solver.add_constraint(A, b, sense="<=")
result = solver.solve(time_limit=60.0)
```

---

## 3. 混合整数规划 (MILP)

```python
from cuopt_solver import MILPSolver, solve_milp
import numpy as np

# 0-1背包问题
values  = np.array([60.0, 100.0, 120.0])
weights = np.array([10.0,  20.0,  30.0])
capacity = 50.0

result = solve_milp(
    c=-values,                           # max → negate for min
    A=weights.reshape(1, -1),
    b=np.array([capacity]),
    vtypes="BBB",                        # binary variables
    sense="minimize",
    time_limit=30.0,
    mip_gap=1e-6
)
selected = result['solution'] > 0.5
print(f"Selected items: {np.where(selected)[0].tolist()}")
print(f"Total value:    {values[selected].sum():.0f}")
print(f"Total weight:   {weights[selected].sum():.0f}")
```

---

## 4. 车辆路径问题 (VRP)

```python
from cuopt_solver import VRPSolver, solve_vrp
import numpy as np

# 5 customers (node 0 = depot)
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

result = solve_vrp(
    cost_matrix=costs,
    n_vehicles=2,
    demands=demands,
    capacities=capacities,
    max_runtime=30.0
)

print(f"Total distance: {result['total_cost']:.2f}")
for i, route in enumerate(result['routes']):
    seq = " -> ".join(map(str, route))
    print(f"  Vehicle {i+1}: {seq}  (cost {result['route_costs'][i]:.2f})")
```

---

## 5. 带时间窗 VRP (VRPTW)

```python
from cuopt_solver import VRPSolver

solver = VRPSolver(n_locations=5, n_vehicles=2)
solver.set_cost_matrix(costs)
solver.set_demands(demands)
solver.set_capacities(capacities)

# time_windows[i] = [earliest_arrival, latest_arrival]
solver.set_time_windows(np.array([
    [0,  100],   # depot
    [10,  30],   # customer 1
    [20,  50],   # customer 2
    [30,  60],   # customer 3
    [40,  80],   # customer 4
]))
solver.set_service_times(np.array([0, 5, 5, 5, 5]))

result = solver.solve(max_runtime=60.0)
```
"""
}
