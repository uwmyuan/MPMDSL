# -*- coding: utf-8 -*-
"""
旅行商问题 (Traveling Salesman Problem, TSP) - 使用 ORMDSL Python 语法糖

问题描述：
- 给定 n 个城市和城市之间的距离矩阵
- 找到访问所有城市一次且仅一次的最短路径
- 返回起点

数学模型：
    minimize sum(i,j in V) { d_ij * x_ij }
    s.t.   sum(j in V) { x_ij } = 1,  for all i in V      (每个城市恰好离开一次)
           sum(i in V) { x_ij } = 1,  for all j in V      (每个城市恰好进入一次)
           sum(i,j in S) { x_ij } <= |S| - 1, for all S subset V, 2 <= |S| <= n-1  (消除子环)
           x_ij in {0, 1}
"""

from sugar import *

# ============================================================
# 1. 数据输入
# ============================================================

# 城市数量
n = 5

# 城市名称
cities = ['A', 'B', 'C', 'D', 'E']

# 距离矩阵 (对称TSP)
# dist[i][j] = 从城市i到城市j的距离
dist = [
    [0,  10, 15, 20, 25],
    [10,  0, 35, 25, 20],
    [15, 35,  0, 30, 15],
    [20, 25, 30,  0, 10],
    [25, 20, 15, 10,  0],
]

def get_dist(i, j):
    """获取城市i到城市j的距离"""
    return dist[i][j]

# ============================================================
# 2. 变量声明
# ============================================================

# 创建索引集合
V = set('V', list(range(n)))  # 所有城市的集合

# 创建二进制决策变量 x[i,j] - 是否从城市i到城市j
x = {}
for i in range(n):
    for j in range(n):
        if i != j:  # 不考虑从城市到自身的转移
            x[(i, j)] = binary(f'x_{i}_{j}')

# 辅助变量：到达城市j的城市（用于MTZ formulation）
u = {}
for j in range(n):
    if j > 0:  # u[0] = 0 或不定义
        u[j] = integer(f'u_{j}', lb=1, ub=n-1)

# ============================================================
# 3. 问题定义
# ============================================================

# 目标函数：最小化总距离
total_distance = Const(0)
for i in range(n):
    for j in range(n):
        if i != j and (i, j) in x:  # 确保变量存在
            total_distance = total_distance + Const(dist[i][j]) * x[(i, j)]

problem = minimize(total_distance)

# ============================================================
# 4. 约束定义
# ============================================================

# 约束1: 每个城市恰好被离开一次 (出度 = 1)
for i in range(n):
    expr = Const(0)
    for j in range(n):
        if i != j:
            expr = expr + x[(i, j)]
    problem += constraint(f'out_degree_{i}', expr == Const(1))

# 约束2: 每个城市恰好被进入一次 (入度 = 1)
for j in range(n):
    expr = Const(0)
    for i in range(n):
        if i != j:
            expr = expr + x[(i, j)]
    problem += constraint(f'in_degree_{j}', expr == Const(1))

# 约束3: MTZ formulation 消除子环
# u[i] - u[j] + n * x[i,j] <= n - 1, for all i != j, i,j > 0
for i in range(1, n):
    for j in range(1, n):
        if i != j:
            # u[i] - u[j] + n*x[i,j] <= n-1
            constraint_expr = u[i] - u[j] + Const(n) * x[(i, j)] <= Const(n - 1)
            problem += constraint(f'mtz_{i}_{j}', constraint_expr)

# ============================================================
# 5. 求解
# ============================================================

print('='*50)
print('旅行商问题 (TSP) 求解')
print('='*50)
print(f'\n城市数量: {n}')
print(f'城市列表: {cities}')
print('\n距离矩阵:')
for i in range(n):
    print(f'  {cities[i]}: {dist[i]}')

print('\n问题模型:')
print('  minimize sum(i,j in V) { d_ij * x_ij }')
print('  s.t.  sum(j) { x_ij } = 1, for all i  (每个城市离开一次)')
print('        sum(i) { x_ij } = 1, for all j  (每个城市进入一次)')
print('        MTZ约束 (消除子环)')

print('\n决策变量:')
print(f'  x[i,j] in {{0,1}}: {n*n - n} 个 (排除对角线)')
print(f'  u[i] in [1,{n-1}]: {n-1} 个 (MTZ变量)')

print('\n求解中...')
result = solve(problem)

print('\n求解结果:')
print(result.summary)

# 提取路径
if result.values:
    print('\n最优路径:')
    path = []
    current = 0  # 从城市0开始
    visited = set([current])
    path.append(current)
    
    while len(visited) < n:
        for j in range(n):
            if j not in visited and x.get((current, j)):
                val = result(f'x_{current}_{j}')
                if val > 0.5:
                    path.append(j)
                    visited.add(j)
                    current = j
                    break
    
    path_str = ' -> '.join([cities[i] for i in path])
    print(f'  {path_str} -> {cities[path[0]]} (返回起点)')
    
    # 计算总距离
    total = sum(dist[path[i]][path[(i+1) % n]] for i in range(n))
    print(f'  总距离: {total}')

# ============================================================
# 简化版本（纯语法糖风格）
# ============================================================

print('\n' + '='*50)
print('简化版本演示')
print('='*50)

print('\n使用语法糖的简洁写法:')
print('  # 定义索引集合')
print('  V = set(\"V\", range(n))')
print('  ')
print('  # 定义边的选择变量')
print('  x = {(i,j): binary(f\"x[{i},{j}]\") for i in range(n) for j in range(n) if i != j}')
print('  ')
print('  # 目标函数')
print('  obj = minimize(sum((i,j), edges, dist[i][j] * x[i,j]))')
print('  ')
print('  # 添加约束')
print('  for i in range(n):')
print('      problem += constraint(f\"out_{i}\", sum(j for j in range(n) if i!=j, x[i,j]) == 1)')


