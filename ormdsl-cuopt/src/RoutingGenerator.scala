package ormopt

/**
 * 路径优化 (VRP / TSP) Python 代码生成器
 *
 * 生成调用 NVIDIA cuOpt Routing 真实 GPU 求解器的 Python 求解脚本，
 * 取代之前基于 ALNS 模板的仿真实现。
 *
 * 后端: cuopt.routing.RoutingData + SolverConfig
 * 安装: pip install nvidia-cuopt
 * 文档: https://docs.nvidia.com/cuopt/user-guide/latest/
 */

import dsl.ormdsl_intepreter._

object RoutingGenerator extends CUOPTCodeGenerator {

  // ── 路径问题类型 ──────────────────────────────────────────────────────────
  sealed trait RoutingProblemType
  case object TSP    extends RoutingProblemType  // Traveling Salesman Problem
  case object CVRP   extends RoutingProblemType  // Capacitated VRP
  case object VRPTW  extends RoutingProblemType  // VRP with Time Windows
  case object VRPPD  extends RoutingProblemType  // VRP with Pickup & Delivery
  case object HCVRP  extends RoutingProblemType  // Heterogeneous Capacitated VRP

  // ── 公共入口 ──────────────────────────────────────────────────────────────

  /**
   * 生成 VRP / CVRP 求解脚本（不带时间窗）。
   *
   * @param problemName  模型名称（用于输出文件命名）
   * @param nLocations   位置数（包含仓库 node 0）
   * @param nVehicles    车辆数
   * @param costMatrix   成本矩阵 (nLocations × nLocations)
   * @param demands      每个位置的需求量 (nLocations,)，仓库=0
   * @param capacities   每辆车的容量 (nVehicles,)
   */
  def generateVRPModel(
    problemName:  String,
    nLocations:   Int,
    nVehicles:    Int,
    costMatrix:   Array[Array[Double]],
    demands:      Array[Double],
    capacities:   Array[Double]
  ): String = {
    val matStr  = formatMatrix(costMatrix)
    val demStr  = formatArray(demands)
    val capStr  = formatArray(capacities)

    s"""#!/usr/bin/env python3
# -*- coding: utf-8 -*-
\"\"\"
ORMDSL-CUOPT  VRP Solver  —  generated
Problem   : $problemName
Locations : $nLocations  (node 0 = depot)
Vehicles  : $nVehicles
Backend   : NVIDIA cuOpt  (pip install nvidia-cuopt)
\"\"\"

import sys, os, json, time
import numpy as np

sys.path.insert(0, os.path.join(os.path.dirname(__file__), ".."))
from src.cuopt_solver import VRPSolver


def build_and_solve(max_runtime: float = 60.0):
    print("=" * 60)
    print("ORMDSL-CUOPT VRP Solver")
    print(f"  Problem  : $problemName")
    print(f"  Locations: $nLocations  (node 0 = depot)")
    print(f"  Vehicles : $nVehicles")
    print("=" * 60)

    # ── Cost matrix ────────────────────────────────────────────────
    costs = np.array($matStr, dtype=np.float64)

    # ── Demands (depot = 0) ────────────────────────────────────────
    demands = np.array($demStr, dtype=np.float64)

    # ── Vehicle capacities ─────────────────────────────────────────
    capacities = np.array($capStr, dtype=np.float64)

    # ── Build solver ───────────────────────────────────────────────
    solver = VRPSolver(n_locations=$nLocations, n_vehicles=$nVehicles)
    solver.set_cost_matrix(costs)
    solver.set_demands(demands)
    solver.set_capacities(capacities)

    # ── Solve ──────────────────────────────────────────────────────
    print(f"\\nSolving with NVIDIA cuOpt Routing (max_runtime={max_runtime}s) ...")
    t0      = time.perf_counter()
    result  = solver.solve(max_runtime=max_runtime)
    elapsed = time.perf_counter() - t0

    # ── Results ────────────────────────────────────────────────────
    print("\\n" + "=" * 60)
    print(f"Status       : {result['status']}")
    print(f"Total cost   : {result['total_cost']:.4f}")
    print(f"Routes used  : {result['num_routes']}")
    print(f"Solve time   : {elapsed:.4f} s")
    print("-" * 60)
    for i, (route, cost) in enumerate(
        zip(result["routes"], result["route_costs"])
    ):
        seq = "0 -> " + " -> ".join(map(str, route)) + " -> 0"
        print(f"  Vehicle {i+1}: [{seq}]  cost={cost:.4f}")
    print("=" * 60)

    # ── Feasibility checks ─────────────────────────────────────────
    _check_feasibility(result, demands, capacities)

    # ── JSON output ────────────────────────────────────────────────
    out = {
        "problem":      "$problemName",
        "status":       result["status"],
        "total_cost":   result["total_cost"],
        "solve_time_s": elapsed,
        "num_routes":   result["num_routes"],
        "routes":       result["routes"],
        "route_costs":  result["route_costs"],
    }
    outfile = "${problemName}_vrp_result.json"
    with open(outfile, "w") as f:
        json.dump(out, f, indent=2)
    print(f"\\nResult written to {{outfile}}")
    return result


def _check_feasibility(result, demands, capacities):
    \"\"\"Verify all routes satisfy capacity constraints.\"\"\"
    ok = True
    for i, route in enumerate(result["routes"]):
        load = sum(demands[c] for c in route)
        cap  = capacities[i] if i < len(capacities) else capacities[-1]
        if load > cap + 1e-6:
            print(f"CAPACITY VIOLATION: vehicle {i+1} load={load:.2f} > cap={cap:.2f}")
            ok = False
    if ok:
        print("Capacity feasibility check: PASSED")


if __name__ == "__main__":
    import argparse
    p = argparse.ArgumentParser()
    p.add_argument("--max-runtime", type=float, default=60.0)
    args = p.parse_args()
    build_and_solve(max_runtime=args.max_runtime)
"""
  }

  /**
   * 生成带时间窗 VRP (VRPTW) 求解脚本。
   */
  def generateVRPTWModel(
    problemName:  String,
    nLocations:   Int,
    nVehicles:    Int,
    costMatrix:   Array[Array[Double]],
    demands:      Array[Double],
    capacities:   Array[Double],
    timeWindows:  Array[Array[Double]],  // (nLocations, 2) — [earliest, latest]
    serviceTimes: Array[Double]          // (nLocations,)
  ): String = {
    val matStr  = formatMatrix(costMatrix)
    val demStr  = formatArray(demands)
    val capStr  = formatArray(capacities)
    val twStr   = formatMatrix(timeWindows)
    val stStr   = formatArray(serviceTimes)

    s"""#!/usr/bin/env python3
# -*- coding: utf-8 -*-
\"\"\"
ORMDSL-CUOPT  VRPTW Solver  —  generated
Problem   : $problemName  (with Time Windows)
Locations : $nLocations  (node 0 = depot)
Vehicles  : $nVehicles
Backend   : NVIDIA cuOpt  (pip install nvidia-cuopt)
\"\"\"

import sys, os, json, time
import numpy as np

sys.path.insert(0, os.path.join(os.path.dirname(__file__), ".."))
from src.cuopt_solver import VRPSolver


def build_and_solve(max_runtime: float = 60.0):
    print("=" * 60)
    print("ORMDSL-CUOPT VRPTW Solver")
    print(f"  Problem  : $problemName")
    print(f"  Locations: $nLocations")
    print(f"  Vehicles : $nVehicles")
    print("=" * 60)

    costs        = np.array($matStr, dtype=np.float64)
    demands      = np.array($demStr, dtype=np.float64)
    capacities   = np.array($capStr, dtype=np.float64)
    time_windows = np.array($twStr,  dtype=np.float64)
    service_times= np.array($stStr,  dtype=np.float64)

    solver = VRPSolver(n_locations=$nLocations, n_vehicles=$nVehicles)
    solver.set_cost_matrix(costs)
    solver.set_demands(demands)
    solver.set_capacities(capacities)
    solver.set_time_windows(time_windows)
    solver.set_service_times(service_times)

    print(f"\\nSolving VRPTW with NVIDIA cuOpt (max_runtime={max_runtime}s) ...")
    t0      = time.perf_counter()
    result  = solver.solve(max_runtime=max_runtime)
    elapsed = time.perf_counter() - t0

    print("\\n" + "=" * 60)
    print(f"Status     : {result['status']}")
    print(f"Total cost : {result['total_cost']:.4f}")
    print(f"Routes     : {result['num_routes']}")
    print(f"Solve time : {elapsed:.4f} s")
    print("-" * 60)
    for i, (route, cost) in enumerate(
        zip(result["routes"], result["route_costs"])
    ):
        seq = "0 -> " + " -> ".join(map(str, route)) + " -> 0"
        print(f"  Vehicle {i+1}: [{seq}]  cost={cost:.4f}")
    print("=" * 60)

    _check_feasibility(result, demands, capacities, time_windows, service_times, costs)

    out = {
        "problem":      "$problemName",
        "status":       result["status"],
        "total_cost":   result["total_cost"],
        "solve_time_s": elapsed,
        "num_routes":   result["num_routes"],
        "routes":       result["routes"],
    }
    outfile = "${problemName}_vrptw_result.json"
    with open(outfile, "w") as f:
        json.dump(out, f, indent=2)
    print(f"\\nResult written to {{outfile}}")
    return result


def _check_feasibility(result, demands, capacities, time_windows, service_times, costs):
    \"\"\"Check capacity + time-window feasibility.\"\"\"
    all_ok = True
    for i, route in enumerate(result["routes"]):
        # Capacity
        load = sum(demands[c] for c in route)
        cap  = capacities[i] if i < len(capacities) else capacities[-1]
        if load > cap + 1e-6:
            print(f"CAP VIOLATION: vehicle {i+1} load={load:.2f} > cap={cap:.2f}")
            all_ok = False

        # Time windows
        cur_time = 0.0
        prev     = 0
        for c in route:
            cur_time += costs[prev][c]
            tw_start, tw_end = time_windows[c]
            if cur_time < tw_start:
                cur_time = tw_start       # wait
            elif cur_time > tw_end + 1e-5:
                print(f"TW VIOLATION: vehicle {i+1} customer {c} arrives {cur_time:.2f} > tw_end {tw_end:.2f}")
                all_ok = False
            cur_time += service_times[c]
            prev = c

    if all_ok:
        print("Feasibility check (capacity + time windows): PASSED")


if __name__ == "__main__":
    import argparse
    p = argparse.ArgumentParser()
    p.add_argument("--max-runtime", type=float, default=60.0)
    args = p.parse_args()
    build_and_solve(max_runtime=args.max_runtime)
"""
  }

  /**
   * 生成 TSP 求解脚本（单辆车、无容量约束）。
   */
  def generateTSPModel(
    problemName: String,
    nLocations:  Int,
    costMatrix:  Array[Array[Double]]
  ): String = {
    val matStr = formatMatrix(costMatrix)

    s"""#!/usr/bin/env python3
# -*- coding: utf-8 -*-
\"\"\"
ORMDSL-CUOPT  TSP Solver  —  generated
Problem   : $problemName
Locations : $nLocations
Backend   : NVIDIA cuOpt  (pip install nvidia-cuopt)
\"\"\"

import sys, os, json, time
import numpy as np

sys.path.insert(0, os.path.join(os.path.dirname(__file__), ".."))
from src.cuopt_solver import VRPSolver   # TSP = VRP with 1 vehicle, no capacity


def build_and_solve(max_runtime: float = 60.0):
    print("=" * 60)
    print("ORMDSL-CUOPT TSP Solver")
    print(f"  Problem  : $problemName")
    print(f"  Locations: $nLocations")
    print("=" * 60)

    costs = np.array($matStr, dtype=np.float64)

    # TSP: one vehicle, unlimited capacity
    solver = VRPSolver(n_locations=$nLocations, n_vehicles=1)
    solver.set_cost_matrix(costs)
    # No demand / capacity constraints for pure TSP

    print(f"\\nSolving TSP with NVIDIA cuOpt (max_runtime={max_runtime}s) ...")
    t0      = time.perf_counter()
    result  = solver.solve(max_runtime=max_runtime)
    elapsed = time.perf_counter() - t0

    print("\\n" + "=" * 60)
    print(f"Status       : {result['status']}")
    print(f"Tour length  : {result['total_cost']:.4f}")
    print(f"Solve time   : {elapsed:.4f} s")
    if result["routes"]:
        tour = result["routes"][0]
        seq  = "0 -> " + " -> ".join(map(str, tour)) + " -> 0"
        print(f"Tour: {seq}")
    print("=" * 60)

    # ── Tour validity ──────────────────────────────────────────────
    if result["routes"]:
        tour     = result["routes"][0]
        visited  = set(tour)
        expected = set(range(1, $nLocations))
        if visited == expected:
            print("Tour validity: PASSED (all customers visited exactly once)")
        else:
            missing = expected - visited
            extra   = visited - expected
            if missing:
                print(f"TOUR ERROR: missing customers {missing}")
            if extra:
                print(f"TOUR ERROR: extra nodes {extra}")

    out = {
        "problem":      "$problemName",
        "status":       result["status"],
        "tour_length":  result["total_cost"],
        "solve_time_s": elapsed,
        "tour":         result["routes"][0] if result["routes"] else [],
    }
    outfile = "${problemName}_tsp_result.json"
    with open(outfile, "w") as f:
        json.dump(out, f, indent=2)
    print(f"\\nResult written to {{outfile}}")
    return result


if __name__ == "__main__":
    import argparse
    p = argparse.ArgumentParser()
    p.add_argument("--max-runtime", type=float, default=60.0)
    args = p.parse_args()
    build_and_solve(max_runtime=args.max_runtime)
"""
  }

  // ── 格式化工具 ────────────────────────────────────────────────────────────

  private def formatMatrix(m: Array[Array[Double]]): String = {
    val rows = m.map(row => "    [" + row.map(v => f"$v%.4g").mkString(", ") + "]")
    "[\n" + rows.mkString(",\n") + "\n]"
  }

  private def formatArray(a: Array[Double]): String =
    "[" + a.map(v => f"$v%.4g").mkString(", ") + "]"
}
