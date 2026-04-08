# ORMDSL-CUOPT Architecture

## Design Overview

Inspired by NVIDIA cuOpt, ORMDSL-CUOPT provides GPU-accelerated optimization solvers through automatic code generation from ORMDSL models.

## System Architecture

```
┌─────────────────────────────────────────────────────────────────┐
│                        ORMDSL Model                             │
│   (Mathematical Programming DSL: LP, MILP, VRP, etc.)           │
└─────────────────────────┬───────────────────────────────────────┘
                          │
                          ▼
┌─────────────────────────────────────────────────────────────────┐
│                     ORMDSL Interpreter                          │
│              (AST → Intermediate Representation)               │
└─────────────────────────┬───────────────────────────────────────┘
                          │
                          ▼
┌─────────────────────────────────────────────────────────────────┐
│                   ORMDSL-CUOPT Code Generator                   │
│  ┌─────────────┐  ┌─────────────┐  ┌─────────────────────────┐   │
│  │ LPGenerator │  │MILPGenerator│  │ RoutingGenerator        │   │
│  │   (PDLP)    │  │   (BnB)     │  │ (ALNS + Local Search)  │   │
│  └─────────────┘  └─────────────┘  └─────────────────────────┘   │
└─────────────────────────┬───────────────────────────────────────┘
                          │
          ┌───────────────┼───────────────┐
          ▼               ▼               ▼
    ┌──────────┐    ┌──────────┐    ┌──────────┐
    │  CUDA    │    │  CUDA    │    │  CUDA    │
    │   LP     │    │  MILP     │    │  VRP     │
    │  Kernels │    │  Kernels  │    │  Kernels │
    └──────────┘    └──────────┘    └──────────┘
          │               │               │
          └───────────────┼───────────────┘
                          ▼
                  ┌──────────────┐
                  │ GPU Runtime  │
                  │  (CUDA)      │
                  └──────────────┘
```

## Core Components

### 1. LP Generator (PDLP)

**Algorithm**: Primal-Dual Linear Programming (PDLP)

**Key Features**:
- GPU-parallel matrix-vector operations
- Adaptive step size selection
- Nesterov acceleration
- Hot-start capability

**Kernels**:
- `pdlp_compute_residuals`: Constraint evaluation
- `pdlp_gradient_step`: Gradient computation
- `pdlp_line_search`: Step size determination
- `pdlp_convergence_check`: Optimality check

### 2. MILP Generator (GPU Branch & Bound)

**Algorithm**: Parallel Branch and Bound with GPU LP solving

**Key Features**:
- Concurrent node processing
- GPU-accelerated LP relaxations
- Best-bound node selection
- Parallel branching

**Kernels**:
- `milp_solve_lp_relaxation`: LP subproblem
- `milp_find_branching_var`: Most fractional variable
- `milp_process_nodes`: Parallel node expansion
- `milp_prune_nodes`: Bound-based pruning

### 3. VRP Generator (ALNS)

**Algorithm**: Adaptive Large Neighborhood Search

**Key Features**:
- Multiple destroy operators (Shaw, random, worst)
- Multiple repair operators (Greedy, regret)
- Simulated annealing acceptance
- Parallel route optimization

**Kernels**:
- `two_opt_evaluate`: 2-opt move evaluation
- `two_opt_apply`: Apply best improvement
- `check_capacity`: Capacity constraint check
- `check_time_windows`: Time window verification

## Data Flow

### LP Solving
```
Initial x ─► Compute residual ─► Gradient ─► Line search ─► Update
     ▲                                                      │
     └──────────────────── converge? ◄──────────────────────┘
```

### MILP Branch & Bound
```
Root ─► Solve LP ─► Branch ─► Child nodes (parallel)
                         │
                    ┌────┴────┐
                    ▼         ▼
                Prune?     Solve LP
                    │         │
                    └────┬────┘
                         ▼
                    Update best
```

### VRP ALNS
```
Current ─► Destroy ─► Repair ─► Evaluate ─► Accept/Reject ─► Update best
                         ▲
                         │
                    ◄─────┘
                    (SA cooling)
```

## Memory Layout

### Dense LP (Row-major)
```
A[m][n] → d_A: float[m * n]
访问模式: A[i * n + j]
```

### Sparse LP (CSR-like)
```
d_values: 非零值数组
d_col_idx: 列索引
d_row_ptr: 行指针
```

### VRP Routes (Blocked)
```
d_routes[max_vehicles][max_route_len]
访问模式: d_routes[v * max_route_len + pos]
```

## Performance Considerations

1. **Memory Coalescing**: Global memory access patterns
2. **Shared Memory**: Thread block data reuse
3. **Warp Divergence**: Branching in kernels
4. **Occupancy**: SM utilization
5. **Host-Device Transfer**: Minimize data movement

## Extension Points

### Adding New Problem Types
1. Implement `ProblemGenerator` trait
2. Add kernel templates
3. Register in `CUOPTMain`

### Adding New Algorithms
1. Create algorithm-specific kernels
2. Add solver state structure
3. Implement main loop
