#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
ORMDSL-CUOPT Python API
GPU-accelerated optimization solver — backed by NVIDIA cuOpt
Falls back to scipy CPU solver when cuOpt is unavailable.

Install GPU backend:
    pip install nvidia-cuopt

Supported problem types:
    - LP   : Linear Programming        (cuopt LP solver / scipy.optimize.linprog)
    - MILP : Mixed-Integer LP          (cuopt MILP solver / scipy.optimize.milp)
    - VRP  : Vehicle Routing Problem   (cuopt Routing solver / greedy CPU fallback)
"""

import numpy as np
from typing import Dict, Any, List, Optional, Union
import warnings

# ── cuOpt availability check ────────────────────────────────────────────────
try:
    from cuopt.linear_programming import problem as lp_module
    from cuopt.linear_programming import solver_settings as ss_module
    _LP_AVAILABLE = True
    _BACKEND = "cuopt-gpu"
except ImportError:
    _LP_AVAILABLE = False
    _BACKEND = "scipy-cpu"

try:
    from cuopt.routing import routing as routing_module
    _ROUTING_AVAILABLE = True
except ImportError:
    _ROUTING_AVAILABLE = False

# ── scipy fallback ───────────────────────────────────────────────────────────
try:
    from scipy.optimize import linprog, milp, LinearConstraint, Bounds
    from scipy.sparse import csc_matrix
    _SCIPY_AVAILABLE = True
except ImportError:
    _SCIPY_AVAILABLE = False

# ── PyTorch GPU availability ──────────────────────────────────────────────────
_TORCH_AVAILABLE = False
_TORCH_GPU_AVAILABLE = False
try:
    import torch as _torch
    _TORCH_AVAILABLE = True
    _TORCH_GPU_AVAILABLE = _torch.cuda.is_available()
except ImportError:
    _torch = None  # type: ignore
    _TORCH_GPU_AVAILABLE = False

_CUOPT_AVAILABLE = _LP_AVAILABLE or _ROUTING_AVAILABLE


def get_backend() -> str:
    """Return currently active backend name."""
    if _LP_AVAILABLE:
        return "cuopt-gpu"
    if _TORCH_GPU_AVAILABLE:
        return f"torch-gpu{_torch.version.cuda}"
    if _TORCH_AVAILABLE:
        return "torch-cpu"
    return "scipy-cpu"


def get_device() -> str:
    """Return device string for PyTorch-based backends."""
    if _TORCH_GPU_AVAILABLE:
        return f"cuda:{_torch.cuda.current_device()}"
    return "cpu"


def _require_lp():
    if not _LP_AVAILABLE and not _SCIPY_AVAILABLE:
        raise ImportError(
            "Neither cuOpt nor scipy is available.\n"
            "Install with:  pip install nvidia-cuopt\n"
            "  or fallback: pip install scipy"
        )


def _require_routing():
    if not _ROUTING_AVAILABLE:
        warnings.warn(
            "cuOpt Routing not available. Using greedy CPU fallback.\n"
            "Install GPU backend: pip install nvidia-cuopt",
            ImportWarning, stacklevel=3
        )


# ── scipy LP fallback ────────────────────────────────────────────────────────

class _ScipyLPSolver:
    """CPU fallback LP solver using scipy.optimize.linprog (HiGHS backend)."""

    def __init__(self, sense: str = "minimize"):
        self._sense = sense.strip().lower()
        self._var_lb: List[float] = []
        self._var_ub: List[float] = []
        self._var_names: List[str] = []
        self._c: Optional[np.ndarray] = None
        self._A_ub: List[np.ndarray] = []
        self._b_ub: List[float] = []
        self._A_eq: List[np.ndarray] = []
        self._b_eq: List[float] = []

    def add_variable(self, name: str, lb: float = 0.0,
                     ub: float = float("inf"), vtype: str = "C") -> int:
        self._var_names.append(name)
        self._var_lb.append(float(lb))
        self._var_ub.append(float(ub))
        return len(self._var_names) - 1

    def set_objective(self, coefficients: np.ndarray,
                      sense: Optional[str] = None) -> None:
        c = np.asarray(coefficients, dtype=np.float64)
        s = (sense or self._sense).strip().lower()
        self._c = c if s == "minimize" else -c

    def add_constraint(self, A: np.ndarray, b: np.ndarray,
                       sense: str = "<=", names=None) -> None:
        A = np.atleast_2d(np.asarray(A, dtype=np.float64))
        b = np.atleast_1d(np.asarray(b, dtype=np.float64))
        for i in range(len(b)):
            row = A[i] if A.shape[0] > 1 else A[0]
            if sense == "<=":
                self._A_ub.append(row)
                self._b_ub.append(b[i])
            elif sense == ">=":
                self._A_ub.append(-row)
                self._b_ub.append(-b[i])
            elif sense == "==":
                self._A_eq.append(row)
                self._b_eq.append(b[i])

    def solve(self, time_limit: float = 3600.0) -> Dict[str, Any]:
        n = len(self._var_names)
        bounds = [(self._var_lb[i],
                   None if np.isinf(self._var_ub[i]) else self._var_ub[i])
                  for i in range(n)]

        A_ub = np.array(self._A_ub) if self._A_ub else None
        b_ub = np.array(self._b_ub) if self._b_ub else None
        A_eq = np.array(self._A_eq) if self._A_eq else None
        b_eq = np.array(self._b_eq) if self._b_eq else None

        res = linprog(
            self._c, A_ub=A_ub, b_ub=b_ub,
            A_eq=A_eq, b_eq=b_eq,
            bounds=bounds, method="highs",
            options={"time_limit": time_limit}
        )

        if res.status == 0:
            status = "optimal"
        elif res.status == 2:
            status = "infeasible"
        elif res.status == 3:
            status = "unbounded"
        else:
            status = "suboptimal"

        obj_sign = -1.0 if self._sense == "maximize" else 1.0
        obj = float(res.fun) * obj_sign if res.fun is not None else float("nan")
        x = np.asarray(res.x, dtype=np.float64) if res.x is not None else np.zeros(n)

        return {
            "status":     status,
            "objective":  obj,
            "solution":   x,
            "iterations": int(getattr(res, "nit", 0)),
            "variables":  dict(zip(self._var_names, x.tolist())),
            "_backend":   "scipy-cpu",
        }


# ── scipy MILP fallback ───────────────────────────────────────────────────────

class _ScipyMILPSolver:
    """CPU fallback MILP solver using scipy.optimize.milp (HiGHS backend)."""

    def __init__(self):
        self._var_lb: List[float] = []
        self._var_ub: List[float] = []
        self._var_names: List[str] = []
        self._vtypes: List[str] = []
        self._c: Optional[np.ndarray] = None
        self._sense: str = "minimize"
        self._constraints: List = []

    def add_variable(self, name: str, lb: float = 0.0,
                     ub: float = float("inf"), vtype: str = "C") -> int:
        self._var_names.append(name)
        self._var_lb.append(float(lb))
        self._var_ub.append(1.0 if vtype == "B" else float(ub))
        self._vtypes.append(vtype)
        return len(self._var_names) - 1

    def set_objective(self, coefficients: np.ndarray,
                      sense: str = "minimize") -> None:
        c = np.asarray(coefficients, dtype=np.float64)
        self._sense = sense.strip().lower()
        self._c = c if self._sense == "minimize" else -c

    def add_constraint(self, A: np.ndarray, b: np.ndarray,
                       sense: str = "<=", names=None) -> None:
        A = np.atleast_2d(np.asarray(A, dtype=np.float64))
        b = np.atleast_1d(np.asarray(b, dtype=np.float64))
        for i in range(len(b)):
            row = A[i] if A.shape[0] > 1 else A[0]
            if sense == "<=":
                self._constraints.append(LinearConstraint(row, -np.inf, b[i]))
            elif sense == ">=":
                self._constraints.append(LinearConstraint(row, b[i], np.inf))
            elif sense == "==":
                self._constraints.append(LinearConstraint(row, b[i], b[i]))

    def solve(self, time_limit: float = 3600.0,
              mip_gap: float = 1e-4) -> Dict[str, Any]:
        n = len(self._var_names)
        integrality = np.array([
            1 if vt in ("I", "B") else 0
            for vt in self._vtypes
        ], dtype=int)

        bounds = Bounds(
            lb=np.array(self._var_lb),
            ub=np.array([1.0 if v == "B" else
                         (np.inf if np.isinf(self._var_ub[i]) else self._var_ub[i])
                         for i, v in enumerate(self._vtypes)])
        )

        options = {
            "time_limit": time_limit,
            "mip_rel_gap": mip_gap,
            "disp": False,
        }

        res = milp(
            c=self._c,
            constraints=self._constraints if self._constraints else None,
            integrality=integrality,
            bounds=bounds,
            options=options,
        )

        if res.status == 0:
            status = "optimal"
        elif res.status == 2:
            status = "infeasible"
        elif res.status == 3:
            status = "gap_exceeded"
        else:
            status = "unknown"

        obj_sign = -1.0 if self._sense == "maximize" else 1.0
        obj = float(res.fun) * obj_sign if res.fun is not None else float("nan")
        x = np.asarray(res.x, dtype=np.float64) if res.x is not None else np.zeros(n)

        return {
            "status":    status,
            "objective": obj,
            "solution":  x,
            "nodes":     int(getattr(res, "mip_node_count", 0)),
            "mip_gap":   float(getattr(res, "mip_dual_bound", mip_gap)),
            "variables": dict(zip(self._var_names, x.tolist())),
            "_backend":  "scipy-cpu",
        }


# ── greedy VRP CPU fallback ──────────────────────────────────────────────────

class _GreedyVRPSolver:
    """
    CPU fallback VRP solver (nearest-neighbor greedy heuristic).
    Only for functional testing when cuOpt is unavailable.
    NOT for production use.
    """

    def __init__(self, n_locations: int, n_vehicles: int):
        self.n_locations = n_locations
        self.n_vehicles = n_vehicles
        self._costs: Optional[np.ndarray] = None
        self._demands: Optional[np.ndarray] = None
        self._capacities: Optional[np.ndarray] = None
        self._time_windows: Optional[np.ndarray] = None
        self._service_times: Optional[np.ndarray] = None

    def set_cost_matrix(self, costs: np.ndarray) -> None:
        self._costs = np.asarray(costs, dtype=np.float64)

    def set_demands(self, demands: np.ndarray) -> None:
        self._demands = np.asarray(demands, dtype=np.float64)

    def set_capacities(self, capacities: np.ndarray) -> None:
        self._capacities = np.asarray(capacities, dtype=np.float64)

    def set_time_windows(self, windows: np.ndarray) -> None:
        self._time_windows = np.asarray(windows, dtype=np.float64)

    def set_service_times(self, service_times: np.ndarray) -> None:
        self._service_times = np.asarray(service_times, dtype=np.float64)

    def solve(self, max_runtime: float = 30.0,
              n_climbers: Optional[int] = None) -> Dict[str, Any]:
        costs = self._costs
        demands = self._demands
        capacities = self._capacities
        n = self.n_locations

        if demands is None:
            demands = np.zeros(n)
        if capacities is None:
            capacities = np.full(self.n_vehicles, np.inf)

        unvisited = set(range(1, n))  # customers (depot = 0)
        routes: List[List[int]] = []
        route_costs: List[float] = []

        for v in range(self.n_vehicles):
            if not unvisited:
                break
            cap = capacities[v] if v < len(capacities) else capacities[-1]
            route: List[int] = []
            load = 0.0
            cur = 0  # start at depot
            cur_time = 0.0

            while unvisited:
                # Find nearest feasible customer
                best_c, best_dist = None, np.inf
                for c in unvisited:
                    if load + demands[c] > cap + 1e-9:
                        continue
                    # Time window check
                    if self._time_windows is not None:
                        arr = cur_time + costs[cur][c]
                        tw_end = self._time_windows[c][1]
                        if arr > tw_end + 1e-4:
                            continue
                    if costs[cur][c] < best_dist:
                        best_dist = costs[cur][c]
                        best_c = c

                if best_c is None:
                    break

                # Update time with time-window waiting
                if self._time_windows is not None:
                    arr = cur_time + costs[cur][best_c]
                    tw_start = self._time_windows[best_c][0]
                    if arr < tw_start:
                        arr = tw_start
                    cur_time = arr
                    if self._service_times is not None:
                        cur_time += self._service_times[best_c]

                route.append(best_c)
                load += demands[best_c]
                cur = best_c
                unvisited.remove(best_c)

            if route:
                cost = 0.0
                prev = 0
                for c in route:
                    cost += costs[prev][c]
                    prev = c
                cost += costs[prev][0]  # return to depot
                routes.append(route)
                route_costs.append(cost)

        # If any customers unvisited, force assign to last vehicle
        if unvisited:
            if routes:
                routes[-1].extend(sorted(unvisited))
            else:
                routes.append(sorted(unvisited))
                route_costs.append(float("inf"))

        total_cost = sum(route_costs)

        return {
            "status":      "feasible",
            "total_cost":  total_cost,
            "num_routes":  len(routes),
            "routes":      routes,
            "route_costs": route_costs,
            "_backend":    "greedy-cpu",
        }


# ═══════════════════════════════════════════════════════════════════════════════
# Public solver classes (auto-select cuOpt GPU or scipy/greedy CPU)
# ═══════════════════════════════════════════════════════════════════════════════

class LPSolver:
    """
    Linear Programming solver.
    Backend priority:
        1. NVIDIA cuOpt GPU   (nvidia-cuopt, Linux/WSL2)
        2. PyTorch GPU PDLP   (torch + CUDA, any OS)
        3. PyTorch CPU PDLP  (torch, fallback)
        4. scipy HiGHS CPU   (fallback)

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

    def __init__(self, sense: str = "minimize", backend: Optional[str] = None):
        _require_lp()
        self._sense = sense.strip().lower()
        # Backend selection
        if backend == "cuopt" and _LP_AVAILABLE:
            self._impl = _CuOptLPSolver(sense)
        elif backend == "scipy":
            self._impl = _ScipyLPSolver(sense)
        elif backend == "torch" and _TORCH_AVAILABLE:
            # Use torch GPU PDLP (for GPU verification)
            import sys, os
            _src_dir = os.path.dirname(os.path.abspath(__file__))
            if _src_dir not in sys.path:
                sys.path.insert(0, _src_dir)
            from torch_gpu_solver import _TorchPDLPSolver
            self._impl = _TorchPDLPSolver(sense)
        elif _LP_AVAILABLE:
            self._impl = _CuOptLPSolver(sense)
        else:
            # Default: scipy HiGHS (proven reliable) over torch PDLP
            self._impl = _ScipyLPSolver(sense)

    def add_variable(self, name: str, lb: float = 0.0,
                     ub: float = float("inf"), vtype: str = "C") -> int:
        return self._impl.add_variable(name, lb, ub, vtype)

    def set_objective(self, coefficients: np.ndarray,
                      sense: Optional[str] = None) -> None:
        return self._impl.set_objective(coefficients, sense)

    def add_constraint(self, A: np.ndarray, b: np.ndarray,
                       sense: str = "<=", names=None) -> None:
        return self._impl.add_constraint(A, b, sense, names)

    def solve(self, time_limit: float = 3600.0) -> Dict[str, Any]:
        result = self._impl.solve(time_limit)
        result.setdefault("_backend", get_backend())
        return result

    @property
    def backend(self) -> str:
        """Return the backend name used by this solver."""
        return get_backend()

    def __repr__(self) -> str:
        return f"LPSolver(backend={get_backend()!r}, sense={self._sense!r})"


class MILPSolver:
    """
    Mixed-Integer LP solver.
    Backend priority:
        1. NVIDIA cuOpt GPU  (nvidia-cuopt)
        2. scipy HiGHS CPU   (scipy.optimize.milp, with GPU-assisted MILP)
    """

    def __init__(self, backend: Optional[str] = None):
        _require_lp()
        if _LP_AVAILABLE:
            self._impl = _CuOptMILPSolver()
        else:
            self._impl = _ScipyMILPSolver()

    def add_variable(self, name: str, lb: float = 0.0,
                     ub: float = float("inf"), vtype: str = "C") -> int:
        return self._impl.add_variable(name, lb, ub, vtype)

    def set_objective(self, coefficients: np.ndarray,
                      sense: str = "minimize") -> None:
        return self._impl.set_objective(coefficients, sense)

    def add_constraint(self, A: np.ndarray, b: np.ndarray,
                       sense: str = "<=", names=None) -> None:
        return self._impl.add_constraint(A, b, sense, names)

    def solve(self, time_limit: float = 3600.0,
              mip_gap: float = 1e-4) -> Dict[str, Any]:
        result = self._impl.solve(time_limit, mip_gap)
        result.setdefault("_backend", _BACKEND)
        return result

    def __repr__(self) -> str:
        return f"MILPSolver(backend={_BACKEND!r})"


class VRPSolver:
    """
    Vehicle Routing Problem solver.
    Uses NVIDIA cuOpt GPU when available, falls back to greedy CPU heuristic.

    Node 0 is the depot; customers are nodes 1 … n.

    Usage::

        solver = VRPSolver(n_locations=6, n_vehicles=2)
        solver.set_cost_matrix(cost_matrix)
        solver.set_demands(demands)
        solver.set_capacities(np.array([15.0, 15.0]))
        result = solver.solve(max_runtime=30.0)
        print(result["total_cost"])
    """

    def __init__(self, n_locations: int, n_vehicles: int):
        if _ROUTING_AVAILABLE:
            self._impl = _CuOptVRPSolver(n_locations, n_vehicles)
        else:
            self._impl = _GreedyVRPSolver(n_locations, n_vehicles)
        self.n_locations = n_locations
        self.n_vehicles = n_vehicles

    def set_cost_matrix(self, costs: np.ndarray) -> None:
        self._impl.set_cost_matrix(costs)

    def set_demands(self, demands: np.ndarray) -> None:
        self._impl.set_demands(demands)

    def set_capacities(self, capacities: np.ndarray) -> None:
        self._impl.set_capacities(capacities)

    def set_time_windows(self, windows: np.ndarray) -> None:
        self._impl.set_time_windows(windows)

    def set_service_times(self, service_times: np.ndarray) -> None:
        self._impl.set_service_times(service_times)

    def solve(self, max_runtime: float = 30.0,
              n_climbers: Optional[int] = None) -> Dict[str, Any]:
        result = self._impl.solve(max_runtime, n_climbers)
        result.setdefault("_backend", "cuopt-gpu" if _ROUTING_AVAILABLE else "greedy-cpu")
        return result

    def __repr__(self) -> str:
        backend = "cuopt-gpu" if _ROUTING_AVAILABLE else "greedy-cpu"
        return (
            f"VRPSolver(n_locations={self.n_locations}, "
            f"n_vehicles={self.n_vehicles}, backend={backend!r})"
        )


# ── cuOpt implementations (used only when cuOpt is installed) ─────────────────

class _CuOptLPSolver:
    """Internal: cuOpt GPU LP solver wrapper."""

    def __init__(self, sense: str = "minimize"):
        self._prob = lp_module.Problem()
        self._vars: List[Any] = []
        self._var_names: List[str] = []
        self._sense = sense.strip().lower()

    def add_variable(self, name: str, lb: float = 0.0,
                     ub: float = float("inf"), vtype: str = "C") -> int:
        ub_val = lp_module.INF if np.isinf(ub) else float(ub)
        v = self._prob.add_variable(name, float(lb), ub_val, vtype)
        self._vars.append(v)
        self._var_names.append(name)
        return len(self._vars) - 1

    def set_objective(self, coefficients: np.ndarray,
                      sense: Optional[str] = None) -> None:
        c = np.asarray(coefficients, dtype=np.float64)
        s = (sense or self._sense).strip().lower()
        obj_expr = sum(float(c[i]) * self._vars[i] for i in range(len(self._vars)))
        obj_sense = lp_module.MAXIMIZE if s == "maximize" else lp_module.MINIMIZE
        self._prob.set_objective(obj_expr, sense=obj_sense)

    def add_constraint(self, A: np.ndarray, b: np.ndarray,
                       sense: str = "<=", names=None) -> None:
        A = np.atleast_2d(np.asarray(A, dtype=np.float64))
        b = np.atleast_1d(np.asarray(b, dtype=np.float64))
        _sm = {"<=": lp_module.LESS_EQUALS,
               ">=": lp_module.GREATER_EQUALS,
               "==": lp_module.EQUALS}
        csense = _sm[sense]
        for i in range(len(b)):
            row = A[i] if A.shape[0] > 1 else A[0]
            expr = sum(float(row[j]) * self._vars[j] for j in range(len(self._vars)))
            cname = names[i] if names and i < len(names) else f"c_{i}"
            self._prob.add_constraint(expr, csense, cname)

    def solve(self, time_limit: float = 3600.0) -> Dict[str, Any]:
        settings = ss_module.SolverSettings()
        settings.time_limit = time_limit
        sol = self._prob.solve(settings=settings)
        _sm = {lp_module.OPTIMAL: "optimal", lp_module.SUBOPTIMAL: "suboptimal",
               lp_module.INFEASIBLE: "infeasible", lp_module.UNBOUNDED: "unbounded"}
        status = _sm.get(sol.status, "unknown")
        x = np.array([sol.get_value(v) for v in self._vars], dtype=np.float64)
        return {
            "status": status, "objective": float(sol.objective_value),
            "solution": x, "iterations": getattr(sol, "iterations", 0),
            "variables": dict(zip(self._var_names, x.tolist())),
        }


class _CuOptMILPSolver:
    """Internal: cuOpt GPU MILP solver wrapper."""

    def __init__(self):
        self._prob = lp_module.Problem()
        self._vars: List[Any] = []
        self._var_names: List[str] = []

    def add_variable(self, name: str, lb: float = 0.0,
                     ub: float = float("inf"), vtype: str = "C") -> int:
        ub_val = lp_module.INF if np.isinf(ub) else float(ub)
        if vtype == "B":
            lb, ub_val = 0.0, 1.0
        v = self._prob.add_variable(name, float(lb), ub_val, vtype)
        self._vars.append(v)
        self._var_names.append(name)
        return len(self._vars) - 1

    def set_objective(self, coefficients: np.ndarray,
                      sense: str = "minimize") -> None:
        c = np.asarray(coefficients, dtype=np.float64)
        obj_expr = sum(float(c[i]) * self._vars[i] for i in range(len(self._vars)))
        obj_sense = (lp_module.MAXIMIZE if sense.strip().lower() == "maximize"
                     else lp_module.MINIMIZE)
        self._prob.set_objective(obj_expr, sense=obj_sense)

    def add_constraint(self, A: np.ndarray, b: np.ndarray,
                       sense: str = "<=", names=None) -> None:
        A = np.atleast_2d(np.asarray(A, dtype=np.float64))
        b = np.atleast_1d(np.asarray(b, dtype=np.float64))
        _sm = {"<=": lp_module.LESS_EQUALS,
               ">=": lp_module.GREATER_EQUALS,
               "==": lp_module.EQUALS}
        csense = _sm[sense]
        for i in range(len(b)):
            row = A[i] if A.shape[0] > 1 else A[0]
            expr = sum(float(row[j]) * self._vars[j] for j in range(len(self._vars)))
            cname = names[i] if names and i < len(names) else f"c_{i}"
            self._prob.add_constraint(expr, csense, cname)

    def solve(self, time_limit: float = 3600.0,
              mip_gap: float = 1e-4) -> Dict[str, Any]:
        settings = ss_module.SolverSettings()
        settings.time_limit = time_limit
        if hasattr(settings, "mip_gap"):
            settings.mip_gap = mip_gap
        sol = self._prob.solve(settings=settings)
        _sm = {lp_module.OPTIMAL: "optimal", lp_module.SUBOPTIMAL: "gap_exceeded",
               lp_module.INFEASIBLE: "infeasible", lp_module.UNBOUNDED: "unbounded"}
        status = _sm.get(sol.status, "unknown")
        x = np.array([sol.get_value(v) for v in self._vars], dtype=np.float64)
        return {
            "status": status, "objective": float(sol.objective_value),
            "solution": x, "nodes": int(getattr(sol, "num_nodes", 0)),
            "mip_gap": float(getattr(sol, "mip_gap", mip_gap)),
            "variables": dict(zip(self._var_names, x.tolist())),
        }


class _CuOptVRPSolver:
    """Internal: cuOpt GPU VRP solver wrapper."""

    def __init__(self, n_locations: int, n_vehicles: int):
        self.n_locations = n_locations
        self.n_vehicles = n_vehicles
        self._data = routing_module.RoutingData(
            n_locations=n_locations, n_vehicles=n_vehicles)

    def set_cost_matrix(self, costs: np.ndarray) -> None:
        self._data.set_cost_matrix(np.asarray(costs, dtype=np.float64))

    def set_demands(self, demands: np.ndarray) -> None:
        self._data.set_demands(np.asarray(demands, dtype=np.float64))

    def set_capacities(self, capacities: np.ndarray) -> None:
        self._data.set_vehicle_capacities(np.asarray(capacities, dtype=np.float64))

    def set_time_windows(self, windows: np.ndarray) -> None:
        self._data.set_time_windows(np.asarray(windows, dtype=np.float64))

    def set_service_times(self, service_times: np.ndarray) -> None:
        self._data.set_service_times(np.asarray(service_times, dtype=np.float64))

    def solve(self, max_runtime: float = 30.0,
              n_climbers: Optional[int] = None) -> Dict[str, Any]:
        config = routing_module.SolverConfig()
        config.max_runtime = max_runtime
        if n_climbers is not None:
            config.number_of_climbers = n_climbers
        sol = self._data.solve(config)
        routes, route_costs = [], []
        for v in range(self.n_vehicles):
            route = sol.get_route(v)
            cost = sol.get_route_cost(v)
            if route:
                routes.append(list(route))
                route_costs.append(float(cost))
        return {
            "status": getattr(sol, "status", "solved"),
            "total_cost": float(sol.total_cost),
            "num_routes": len(routes),
            "routes": routes, "route_costs": route_costs,
        }


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
    """Solve  min/max c·x  s.t. A·x <= b,  lb <= x <= ub."""
    c = np.asarray(c, dtype=np.float64)
    n = len(c)
    lb = np.zeros(n) if var_lb is None else np.asarray(var_lb, dtype=np.float64)
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
    """Solve MILP."""
    c = np.asarray(c, dtype=np.float64)
    n = len(c)
    lb = np.zeros(n) if var_lb is None else np.asarray(var_lb, dtype=np.float64)
    ub = np.full(n, np.inf) if var_ub is None else np.asarray(var_ub, dtype=np.float64)

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
    """Solve a Vehicle Routing Problem."""
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
