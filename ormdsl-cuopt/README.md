# ORMDSL-CUOPT

GPU-accelerated optimization solver code generator for ORMDSL.

## Overview

This module generates CUDA/C++ code for GPU-accelerated optimization, inspired by NVIDIA cuOpt.

## Supported Problem Types

| Problem Type | Algorithm | GPU Optimization |
|-------------|----------|------------------|
| Linear Programming (LP) | PDLP | ✅ Batch solving |
| Quadratic Programming (QP) | Conjugate Gradient | ✅ Multi-GPU |
| Mixed Integer LP (MILP) | Branch & Bound | ✅ Parallel branching |
| Vehicle Routing (VRP) | Tabu Search + ALNS | ✅ GPU-accelerated |
| Traveling Salesman (TSP) | Lin-Kernighan | ✅ GPU parallelism |

## Architecture

```
ormdsl-cuopt/
├── src/
│   ├── CUOPTCodeGenerator.scala    # Main code generator
│   ├── IR2CUOPT.scala               # IR to CUDA conversion
│   ├── LPGenerator.scala            # Linear programming generator
│   ├── MILPGenerator.scala         # Mixed integer LP generator
│   ├── RoutingGenerator.scala       # VRP/TSP generator
│   └── KernelTemplates.scala        # CUDA kernel templates
└── README.md
```

## Features

### 1. LP/QP Generation
- Generates PDLP (Primal-Dual Linear Programming) solver
- Multi-GPU support for large-scale problems
- Hot-start capability

### 2. MILP Generation
- GPU-accelerated branch and bound
- Parallel LP relaxation solving
- Incumbent solution callbacks

### 3. Routing Optimization
- GPU-parallel VRP solver
- Multi-objective optimization (distance, vehicles, time)
- Heterogeneous fleet support

## Usage

```python
# Generated Python API
from cuopt_solver import solve_lp, solve_milp, solve_vrp

# LP example
result = solve_lp(c, A, b, sense="min")
print(f"Optimal value: {result.obj_val}")

# VRP example
result = solve_vrp(cost_matrix, num_vehicles=10)
print(f"Routes: {result.routes}")
```

## Requirements

- CUDA Toolkit 11.0+
- cuOpt SDK (for runtime execution)
- NumPy + CuPy (for GPU arrays)
