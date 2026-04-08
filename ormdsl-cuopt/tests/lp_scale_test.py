#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
ORMDSL-CUOPT CPU vs GPU 大规模LP性能对比测试
==============================================
测试不同规模LP问题的CPU与GPU求解时间

运行方式:
  # CPU测试 (Windows)
  python tests/lp_scale_test.py --cpu

  # GPU测试 (Docker)
  docker run --gpus all --rm -v "$(pwd):/workspace" \
    nvidia/cuopt:25.5.0-cuda12.8-py312 \
    python3 /workspace/tests/lp_scale_test.py --gpu

  # 或本地 (如果同时有scipy和cuOpt)
  python tests/lp_scale_test.py --both
"""

import sys
import os
import time
import argparse
import numpy as np
import json
from datetime import datetime

# 路径设置
ROOT_DIR = os.path.dirname(os.path.dirname(os.path.abspath(__file__)))
SRC_DIR = os.path.join(ROOT_DIR, "src")
sys.path.insert(0, ROOT_DIR)
sys.path.insert(0, SRC_DIR)

# 结果存储
RESULTS = []

# ═══════════════════════════════════════════════════════════════════════════════
# 求解器检测
# ═══════════════════════════════════════════════════════════════════════════════

def check_backends():
    """检测可用的后端"""
    backends = {"cpu": False, "gpu": False}

    # CPU (scipy HiGHS)
    try:
        from scipy.optimize import linprog
        backends["cpu"] = True
        print("[OK] CPU backend: scipy HiGHS available")
    except ImportError:
        print("[SKIP] CPU backend: scipy not available")

    # GPU (cuOpt)
    try:
        from cuopt.linear_programming import DataModel, Solve, SolverSettings
        backends["gpu"] = True
        print("[OK] GPU backend: NVIDIA cuOpt available")
    except ImportError:
        print("[SKIP] GPU backend: cuOpt not available")

    return backends

# ═══════════════════════════════════════════════════════════════════════════════
# CPU求解 (scipy HiGHS)
# ═══════════════════════════════════════════════════════════════════════════════

def solve_cpu_lp(c, A, b, time_limit=120.0):
    """使用scipy HiGHS求解器 (CPU)"""
    from scipy.optimize import linprog

    t0 = time.perf_counter()

    try:
        # 使用严格的变量下界 0.1 确保解非零
        result = linprog(
            c=c,  # scipy是最小化
            A_ub=A,
            b_ub=b,
            bounds=[(0.1, 10)] * len(c),  # 严格下界 0.1
            method='highs'
        )

        elapsed = time.perf_counter() - t0

        if result.success:
            return {
                "status": "optimal",
                "objective": result.fun,
                "time": elapsed
            }
        else:
            return {
                "status": "failed",
                "objective": None,
                "time": elapsed,
                "message": str(result.message) if hasattr(result, 'message') else "Unknown error"
            }
    except Exception as e:
        elapsed = time.perf_counter() - t0
        return {
            "status": "error",
            "objective": None,
            "time": elapsed,
            "message": str(e)
        }

# ═══════════════════════════════════════════════════════════════════════════════
# GPU求解 (cuOpt)
# ═══════════════════════════════════════════════════════════════════════════════

def solve_gpu_lp(c, A, b, time_limit=120.0):
    """使用cuOpt求解器 (GPU)"""
    from cuopt.linear_programming import DataModel, Solve, SolverSettings

    m, n = A.shape

    # 构建CSR格式
    A_values = []
    A_indices = []
    A_offsets = [0]

    for i in range(m):
        row_start = len(A_values)
        for j in range(n):
            val = A[i, j]
            if abs(val) > 1e-12:
                A_values.append(float(val))
                A_indices.append(j)
        A_offsets.append(len(A_values))

    A_values = np.array(A_values, dtype=np.float64)
    A_indices = np.array(A_indices, dtype=np.int32)
    A_offsets = np.array(A_offsets, dtype=np.int32)

    # 创建cuOpt DataModel
    dm = DataModel()
    dm.set_objective_coefficients(c)
    dm.set_csr_constraint_matrix(A_values, A_indices, A_offsets)
    dm.set_constraint_upper_bounds(b.astype(np.float64))
    dm.set_constraint_lower_bounds(np.full(m, -np.inf, dtype=np.float64))
    # 设置变量下界 0.1（与CPU一致）
    dm.set_variable_lower_bounds(np.full(n, 0.1, dtype=np.float64))
    dm.set_variable_upper_bounds(np.full(n, 10.0, dtype=np.float64))

    # 设置求解器
    settings = SolverSettings()
    settings.time_limit = time_limit

    t0 = time.perf_counter()
    sol = Solve(dm, settings)
    elapsed = time.perf_counter() - t0

    status_map = {
        "Optimal": "optimal",
        "PrimalInfeasible": "infeasible",
        "DualInfeasible": "unbounded",
    }
    raw_status = sol.get_termination_reason()
    status = status_map.get(raw_status, raw_status)

    return {
        "status": status,
        "objective": sol.get_primal_objective() if status == "optimal" else None,
        "time": elapsed
    }

# ═══════════════════════════════════════════════════════════════════════════════
# 生成测试问题
# ═══════════════════════════════════════════════════════════════════════════════

def generate_lp_problem(n, m, seed=42):
    """生成随机LP问题（保证可行且有非平凡解）"""
    rng = np.random.default_rng(seed)

    # 目标函数系数
    c = rng.uniform(0.1, 1, n)

    # 约束矩阵：完全密集的小规模块 + 稀疏的大规模部分
    density = min(1.0, max(0.1, 30 / n))  # 至少10%密度
    A = rng.uniform(0, 1, (m, n)) * (rng.uniform(0, 1, (m, n)) < density)

    # 生成可行解 x0（所有变量都为正）
    x0 = rng.uniform(1.0, 5.0, n)

    # 约束右端项
    Ax = A @ x0
    # 添加20%余量
    slack = 0.2 * np.abs(Ax)
    slack = np.maximum(slack, 0.1)  # 至少0.1
    b = Ax + slack

    return c, A, b

# ═══════════════════════════════════════════════════════════════════════════════
# 运行测试
# ═══════════════════════════════════════════════════════════════════════════════

def run_scale_test(sizes, backends, n_runs=3):
    """运行规模测试"""
    print("\n" + "="*70)
    print("大规模LP性能对比测试")
    print("="*70)
    print(f"测试规模: {sizes}")
    print(f"每次规模运行次数: {n_runs}")
    print(f"后端: {backends}")
    print("="*70 + "\n")

    for n, m in sizes:
        print(f"\n{'='*60}")
        print(f"  规模: n={n}, m={m}  ({m/n:.1%} 约束密度)")
        print(f"{'='*60}")

        for run in range(n_runs):
            seed = run + 1
            c, A, b = generate_lp_problem(n, m, seed=seed)

            print(f"\n  [Run {run+1}/{n_runs}]")

            # CPU测试
            if backends["cpu"]:
                result_cpu = solve_cpu_lp(c, A, b)
                obj_str = f"{result_cpu['objective']:+.4f}" if result_cpu["objective"] is not None else "N/A"
                print(f"    CPU (HiGHS):  status={result_cpu['status']:<12} "
                      f"obj={obj_str}  time={result_cpu['time']:.4f}s")
                RESULTS.append({
                    "scale": f"{n}x{m}",
                    "run": run + 1,
                    "backend": "CPU (HiGHS)",
                    "status": result_cpu["status"],
                    "objective": result_cpu["objective"],
                    "time_s": result_cpu["time"]
                })

            # GPU测试
            if backends["gpu"]:
                result_gpu = solve_gpu_lp(c, A, b)
                print(f"    GPU (cuOpt):  status={result_gpu['status']:<12} "
                      f"obj={result_gpu['objective']:+.4f}  time={result_gpu['time']:.4f}s")
                RESULTS.append({
                    "scale": f"{n}x{m}",
                    "run": run + 1,
                    "backend": "GPU (cuOpt)",
                    "status": result_gpu["status"],
                    "objective": result_gpu["objective"],
                    "time_s": result_gpu["time"]
                })

# ═══════════════════════════════════════════════════════════════════════════════
# 主程序
# ═══════════════════════════════════════════════════════════════════════════════

def main():
    parser = argparse.ArgumentParser(description="LP规模性能测试")
    parser.add_argument("--cpu", action="store_true", help="仅运行CPU测试")
    parser.add_argument("--gpu", action="store_true", help="仅运行GPU测试")
    parser.add_argument("--both", action="store_true", help="同时运行CPU和GPU测试")
    parser.add_argument("--runs", type=int, default=3, help="每次规模运行次数")
    parser.add_argument("--output", type=str, help="输出JSON文件路径")
    args = parser.parse_args()

    print(f"\n测试时间: {datetime.now().strftime('%Y-%m-%d %H:%M:%S')}")

    # 检测后端
    backends = check_backends()

    # 确定运行哪些后端
    if args.cpu:
        backends["gpu"] = False
    elif args.gpu:
        backends["cpu"] = False
    elif not args.both:
        # 默认: 运行所有可用后端
        pass

    if not backends["cpu"] and not backends["gpu"]:
        print("\n[ERROR] No backend available!")
        return 1

    # 测试规模 (n变量, m约束)
    # 调整规模以确保scipy能成功求解
    sizes = [
        # 小规模
        (50, 25),      # 50×25
        (100, 50),     # 100×50
        # 中规模
        (200, 100),    # 200×100
        (500, 200),    # 500×200
        (1000, 400),   # 1000×400
        # 大规模
        (2000, 800),   # 2000×800
        (5000, 1000),  # 5000×1000
    ]

    # 运行测试
    run_scale_test(sizes, backends, n_runs=args.runs)

    # 打印汇总
    print("\n" + "="*70)
    print("测试结果汇总")
    print("="*70)

    if backends["cpu"] and backends["gpu"]:
        # 计算CPU vs GPU对比
        scales = sorted(set(r["scale"] for r in RESULTS))
        print(f"\n{'规模':<12} {'CPU时间':<12} {'GPU时间':<12} {'加速比':<10} {'胜出':<8}")
        print("-" * 54)

        for scale in scales:
            cpu_times = [r["time_s"] for r in RESULTS
                        if r["scale"] == scale and r["backend"] == "CPU (HiGHS)"]
            gpu_times = [r["time_s"] for r in RESULTS
                        if r["scale"] == scale and r["backend"] == "GPU (cuOpt)"]

            if cpu_times and gpu_times:
                avg_cpu = sum(cpu_times) / len(cpu_times)
                avg_gpu = sum(gpu_times) / len(gpu_times)
                speedup = avg_cpu / avg_gpu if avg_gpu > 0 else float('inf')
                winner = "GPU" if speedup > 1 else "CPU"
                print(f"{scale:<12} {avg_cpu:>10.4f}s {avg_gpu:>10.4f}s "
                      f"{speedup:>8.2f}x {winner:<8}")
    else:
        # 仅单一后端
        print(f"\n{'规模':<12} {'后端':<15} {'平均时间':<12} {'最小':<12} {'最大':<12}")
        print("-" * 63)

        scales = sorted(set(r["scale"] for r in RESULTS))
        for scale in scales:
            times = [r["time_s"] for r in RESULTS if r["scale"] == scale]
            backend = RESULTS[next(i for i, r in enumerate(RESULTS) if r["scale"] == scale)]["backend"]
            print(f"{scale:<12} {backend:<15} "
                  f"{sum(times)/len(times):>10.4f}s {min(times):>10.4f}s {max(times):>10.4f}s")

    # 保存结果
    if args.output:
        output = {
            "timestamp": datetime.now().isoformat(),
            "test_config": {
                "sizes": sizes,
                "n_runs": args.runs,
                "backends": {k: v for k, v in backends.items() if v}
            },
            "results": RESULTS
        }
        with open(args.output, "w", encoding="utf-8") as f:
            json.dump(output, f, indent=2, ensure_ascii=False)
        print(f"\n[OK] Results saved to: {args.output}")

    return 0

if __name__ == "__main__":
    sys.exit(main())
