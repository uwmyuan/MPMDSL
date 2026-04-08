# -*- coding: utf-8 -*-
"""
GPU-accelerated LP solver using PyTorch CUDA.
Fallback implementation when cuOpt is not available.

Uses two methods:
1. Projected Gradient Descent (PGD) - fast for small/medium LP
2. Augmented Lagrangian (AL) - reliable for difficult constraints

Both run on GPU via PyTorch CUDA tensors.
"""

import numpy as np
from typing import Dict, Any, List, Optional
import time as _time

# ── PyTorch GPU availability ─────────────────────────────────────────────────
_TORCH_AVAILABLE = False
_TORCH_GPU_AVAILABLE = False

try:
    import torch
    _TORCH_AVAILABLE = True
    _TORCH_GPU_AVAILABLE = torch.cuda.is_available()
    _DEVICE = torch.device("cuda" if _TORCH_GPU_AVAILABLE else "cpu")
except ImportError:
    torch = None  # type: ignore
    _DEVICE = None


def get_torch_backend() -> str:
    if _TORCH_GPU_AVAILABLE:
        return f"torch-cuda{torch.version.cuda}"
    elif _TORCH_AVAILABLE:
        return "torch-cpu"
    return "none"


# ── GPU LP Solver ─────────────────────────────────────────────────────────────

class _TorchPDLPSolver:
    """
    GPU LP solver using Projected Gradient Descent + Augmented Lagrangian.

    Works by:
    1. PGD: x_new = proj_C(x - tau * grad)
    2. AL: penalty on constraint violations
    3. Bisection on step size to ensure sufficient descent
    """

    def __init__(
        self,
        sense: str = "minimize",
        device: Optional[str] = None,
    ):
        self._sense = sense.strip().lower()
        self._var_lb: List[float] = []
        self._var_ub: List[float] = []
        self._var_names: List[str] = []
        self._c: Optional[np.ndarray] = None
        self._A_ub: List[np.ndarray] = []
        self._b_ub: List[float] = []
        self._A_eq: List[np.ndarray] = []
        self._b_eq: List[float] = []

        if device == "cpu":
            self._device = torch.device("cpu")
        elif device == "cuda" and _TORCH_GPU_AVAILABLE:
            self._device = torch.device("cuda")
        else:
            self._device = _DEVICE

        self._torch_gpu = self._device.type == "cuda"

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

    def solve(
        self,
        time_limit: float = 60.0,
        max_iter: int = 100000,
        tol: float = 1e-6,
    ) -> Dict[str, Any]:
        if self._c is None:
            raise ValueError("set_objective() not called")

        n = len(self._var_names)
        m_ub = len(self._A_ub)
        m_eq = len(self._A_eq)

        # ── 无约束 ───────────────────────────────────────────────────────
        if m_ub == 0 and m_eq == 0:
            x_star = np.clip(
                -self._c * (1 if self._sense == "minimize" else -1),
                self._var_lb, self._var_ub)
            obj = float(self._c.dot(x_star))
            if self._sense == "maximize":
                obj = -obj
            return {
                "status": "optimal", "objective": obj, "solution": x_star,
                "iterations": 0, "variables": dict(zip(self._var_names, x_star.tolist())),
                "_backend": "torch-gpu" if self._torch_gpu else "torch-cpu",
                "_device": str(self._device),
            }

        # ── 构建 PyTorch 张量 ─────────────────────────────────────────────
        t_c = torch.tensor(self._c, dtype=torch.float64, device=self._device)
        t_lb = torch.tensor(self._var_lb, dtype=torch.float64, device=self._device)
        t_ub = torch.tensor(self._var_ub, dtype=torch.float64, device=self._device)

        if m_ub > 0:
            t_A = torch.tensor(np.array(self._A_ub), dtype=torch.float64, device=self._device)
            t_b = torch.tensor(np.array(self._b_ub), dtype=torch.float64, device=self._device)
            m = m_ub
        else:
            t_A = torch.tensor(np.array(self._A_eq), dtype=torch.float64, device=self._device)
            t_b = torch.tensor(np.array(self._b_eq), dtype=torch.float64, device=self._device)
            m = m_eq

        # ── 计算 Lipschitz 常数 (用于步长) ─────────────────────────────────
        L = float(torch.linalg.norm(t_A, p=2))
        if L < 1e-10:
            L = 1.0

        # ── 投影梯度下降 (PGD) + Nesterov 加速 ───────────────────────────
        # tau = 1/L 是梯度下降的安全步长上界
        tau = 0.95 / L
        # momentum
        theta = 0.9

        # ── 初始化：使用贪心可行点 ─────────────────────────────────────────
        # 找到一个满足约束的初始点
        x = self._find_feasible_start(n, m, t_A, t_b, t_lb, t_ub)
        x_hat = x.clone()

        prev_obj = float("inf")
        best_obj = float("inf")
        best_x = x.clone()
        start = _time.time()

        for k in range(max_iter):
            if _time.time() - start > time_limit:
                break

            # 目标函数梯度
            grad = t_c

            # PGD 更新
            x_new = x_hat - tau * grad
            x_new = torch.clamp(x_new, t_lb, t_ub)

            # 约束违反度
            Ax_new = t_A @ x_new
            viol = Ax_new - t_b  # 对 <= 约束: viol > 0 违反
            viol_loss = float(torch.clamp(viol, min=0.0).norm())

            # AL 惩罚: 如果有违反，减小步长
            if viol_loss > 1e-8:
                # 可行性导向步长
                alpha = torch.clamp(viol / (t_A @ x_hat - t_b + 1e-10), max=1.0)
                x_new = x_hat - tau * 0.1 * (grad + t_A.t() @ alpha)

            # Nesterov 加速
            x_hat = x_new + theta * (x_new - x)
            x = x_new

            # 跟踪最优可行目标
            if viol_loss < 1e-4:
                obj_val = float(t_c.dot(x))
                if obj_val < best_obj:
                    best_obj = obj_val
                    best_x = x.clone()

            # 收敛检查 (每 500 步)
            if k % 500 == 0 or k == max_iter - 1:
                obj_val = float(t_c.dot(x))
                obj_change = abs(obj_val - prev_obj)
                prev_obj = obj_val

                if viol_loss < tol and obj_change < tol * (1.0 + abs(obj_val)):
                    break

        # ── 结果 ──────────────────────────────────────────────────────────
        final_viol = float((torch.clamp(t_A @ x - t_b, min=0.0).norm()))
        if final_viol > 1e-3 and best_obj < float("inf"):
            # 使用找到的最优可行解
            final_x = best_x
            final_obj = best_obj
        else:
            final_x = x
            final_obj = float(t_c.dot(x))

        x_np = final_x.detach().cpu().numpy()
        if self._sense == "maximize":
            final_obj = -final_obj

        max_violation = float(torch.clamp(t_A @ final_x - t_b, min=0.0).max().item())
        status = "optimal" if max_violation < 1e-3 else "suboptimal"

        return {
            "status": status,
            "objective": final_obj,
            "solution": x_np,
            "iterations": k + 1,
            "max_violation": max_violation,
            "variables": dict(zip(self._var_names, x_np.tolist())),
            "_backend": "torch-gpu" if self._torch_gpu else "torch-cpu",
            "_device": str(self._device),
        }

    def _find_feasible_start(
        self, n: int, m: int,
        t_A: 'torch.Tensor', t_b: 'torch.Tensor',
        t_lb: 'torch.Tensor', t_ub: 'torch.Tensor'
    ) -> 'torch.Tensor':
        """
        使用贪心投影找到满足约束的初始点。
        从原点出发，逐步投影到约束半空间。
        """
        x = (t_lb + t_ub) * 0.5  # 中点

        for _ in range(100):
            Ax = t_A @ x
            viol = Ax - t_b  # viol[i] > 0: 违反约束 i
            if float(viol.max()) <= 1e-8:
                break
            # 找到最违反的约束
            worst = torch.argmax(viol)
            # 投影到最违反的半空间
            # a^T x = b => x += (b - a^T x) * a / ||a||^2
            a = t_A[worst]
            residual = t_b[worst] - torch.dot(a, x)
            a_norm_sq = torch.dot(a, a) + 1e-10
            x = x + (residual / a_norm_sq) * a
            x = torch.clamp(x, t_lb, t_ub)

        return x.detach().clone()


class _TorchBranchBoundMILP:
    """MILP: 使用 scipy HiGHS (scipy 内部有 HiGHS C++ 库)"""

    def __init__(self, device: Optional[str] = None):
        self._device = device or ("cuda" if _TORCH_GPU_AVAILABLE else "cpu")
        self._torch_gpu = self._device == "cuda"

    def solve(
        self,
        c: np.ndarray,
        A: np.ndarray,
        b: np.ndarray,
        vtypes: np.ndarray,
        var_lb: np.ndarray,
        var_ub: np.ndarray,
        time_limit: float = 60.0,
        node_limit: int = 1000,
    ) -> Dict[str, Any]:
        from scipy.optimize import milp, LinearConstraint, Bounds

        integrality = vtypes.astype(int)
        bounds = Bounds(lb=var_lb, ub=var_ub)
        constraints = [LinearConstraint(A[i], -np.inf, b[i]) for i in range(len(b))]

        res = milp(
            c=-c,
            constraints=constraints,
            integrality=integrality,
            bounds=bounds,
            options={"time_limit": time_limit, "node_limit": node_limit},
        )

        if res.status == 0:
            status = "optimal"
        elif res.status == 2:
            status = "infeasible"
        else:
            status = "suboptimal"

        x = np.asarray(res.x) if res.x is not None else np.zeros_like(c)
        return {
            "status": status,
            "objective": float(res.fun) if res.fun is not None else float("nan"),
            "solution": x,
            "nodes": int(getattr(res, "mip_node_count", 0)),
            "mip_gap": float(getattr(res, "mip_dual_bound", 0)),
            "_backend": "scipy-cpu",
            "_device": self._device,
        }


def solve_torch_lp_gpu(
    c: np.ndarray,
    A: np.ndarray,
    b: np.ndarray,
    sense: str = "minimize",
    var_lb: Optional[np.ndarray] = None,
    var_ub: Optional[np.ndarray] = None,
    time_limit: float = 60.0,
    device: str = "cuda",
) -> Dict[str, Any]:
    solver = _TorchPDLPSolver(sense=sense, device=device)
    n = len(c)
    lb = np.zeros(n) if var_lb is None else np.asarray(var_lb)
    ub = np.full(n, np.inf) if var_ub is None else np.asarray(var_ub)
    for i in range(n):
        solver.add_variable(f"x_{i}", lb=lb[i], ub=ub[i])
    solver.set_objective(c)
    solver.add_constraint(A, b)
    return solver.solve(time_limit=time_limit)
