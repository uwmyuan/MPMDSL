# -*- coding: utf-8 -*-
"""
P-中位数问题 (P-Median Problem) - 使用 ORMDSL Python 语法糖

问题描述：
- 给定 n 个候选设施点和 m 个客户点
- 选择 p 个设施点开门
- 每个客户分配到最近的开门设施
- 最小化总加权距离

数学模型：
    minimize sum(i in C, j in F) { w_i * d_ij * x_ij }
    s.t.   sum(j in F) { x_ij } = 1,  for all i in C     (每个客户分配到一个设施)
           x_ij <= y_j,                    for all i in C, j in F  (只有开门设施才能服务客户)
           sum(j in F) { y_j } = p,                  (恰好选择p个设施)
           x_ij in {0, 1}, y_j in {0, 1}

扩展问题:
1. R-中断 (Interdiction): 找出r个设施进行破坏，使总成本最大
2. Q-强化 (Fortification): 在r个破坏的基础上，强化q个设施
"""

from sugar import *

# ============================================================
# 1. 基础P-中位数问题
# ============================================================

print('='*60)
print('P-中位数问题 (P-Median Problem)')
print('='*60)

# 数据输入
num_customers = 8   # 客户数量
num_facilities = 5  # 设施数量
p = 2               # 要选择的设施数量

# 客户数据 (weight: 权重/需求量)
customer_weights = [10, 15, 8, 12, 20, 5, 18, 11]

# 距离矩阵 (客户i到设施j的距离)
distance = [
    [4, 8, 6, 10, 12],   # 客户0
    [6, 4, 8, 6, 10],    # 客户1
    [10, 12, 4, 8, 6],    # 客户2
    [8, 6, 10, 4, 8],     # 客户3
    [12, 10, 8, 6, 4],    # 客户4
    [6, 8, 10, 12, 6],    # 客户5
    [10, 6, 12, 8, 10],   # 客户6
    [8, 10, 6, 10, 8],    # 客户7
]

# 集合定义
C = set('C', list(range(num_customers)))  # 客户集合
F = set('F', list(range(num_facilities))) # 设施集合

# 决策变量
# x[i,j] = 1 如果客户i分配给设施j
x = {}
for i in range(num_customers):
    for j in range(num_facilities):
        x[(i, j)] = binary(f'x_{i}_{j}')

# y[j] = 1 如果设施j开门
y = {}
for j in range(num_facilities):
    y[j] = binary(f'y_{j}')

# 目标函数: 最小化总加权距离
total_cost = Const(0)
for i in range(num_customers):
    for j in range(num_facilities):
        total_cost = total_cost + Const(customer_weights[i] * distance[i][j]) * x[(i, j)]

problem = minimize(total_cost)

# 约束1: 每个客户恰好分配到一个设施
for i in range(num_customers):
    expr = Const(0)
    for j in range(num_facilities):
        expr = expr + x[(i, j)]
    problem += constraint(f'assign_{i}', expr == Const(1))

# 约束2: 客户只能分配给开门设施
for i in range(num_customers):
    for j in range(num_facilities):
        # x[i,j] <= y[j]  -> x[i,j] - y[j] <= 0
        constraint_expr = x[(i, j)] - y[j] <= Const(0)
        problem += constraint(f'serve_{i}_{j}', constraint_expr)

# 约束3: 恰好选择p个设施
expr = Const(0)
for j in range(num_facilities):
    expr = expr + y[j]
problem += constraint('num_facilities', expr == Const(p))

print(f'\n基础数据:')
print('-'*40)
print(f'客户数量: {num_customers}')
print(f'设施数量: {num_facilities}')
print(f'选择数量: {p}')

print('\n客户权重:')
print(f'  {customer_weights}')

print('\n距离矩阵:')
for i in range(num_customers):
    print(f'  客户{i}: {distance[i]}')

print('\n问题模型:')
print('  minimize sum(i in C, j in F) { w_i * d_ij * x_ij }')
print('  s.t.  sum(j) { x_ij } = 1, for all i  (每个客户分配到一个设施)')
print('        x_ij <= y_j, for all i,j      (只能分配给开门设施)')
print('        sum(j) { y_j } = p             (选择p个设施)')

print('\n求解中...')
result = solve(problem)

print('\n求解结果:')
print(result.summary)

# 提取结果
if result.values:
    print('\n最优解:')
    print('-'*40)
    
    # 找出开门的设施
    open_facilities = []
    for j in range(num_facilities):
        if result(f'y_{j}') > 0.5:
            open_facilities.append(j)
    print(f'开门设施: {[f"F{j}" for j in open_facilities]}')
    
    # 客户分配
    print('\n客户分配:')
    for i in range(num_customers):
        for j in range(num_facilities):
            if result(f'x_{i}_{j}') > 0.5:
                cost = customer_weights[i] * distance[i][j]
                print(f'  客户{i} (权重={customer_weights[i]}) -> 设施{j} (距离={distance[i][j]}), 加权距离={cost}')
    print('-'*40)

# ============================================================
# 扩展1: R-中断问题 (Interdiction)
# ============================================================

print('\n' + '='*60)
print('扩展1: R-中断问题 (R-Interdiction)')
print('='*60)

r = 1  # 破坏设施数量

# 中断后的距离（破坏设施后，客户需要去更远的设施）
# 简化：假设破坏设施后，距离变为原来的2倍
distance_interdicted = [
    [8, 16, 12, 10, 12],   # 客户0
    [12, 8, 16, 6, 10],    # 客户1
    [20, 24, 8, 8, 6],     # 客户2
    [16, 12, 20, 8, 8],    # 客户3
    [24, 20, 16, 6, 4],    # 客户4
    [12, 16, 20, 12, 6],   # 客户5
    [20, 12, 24, 8, 10],   # 客户6
    [16, 20, 12, 10, 8],   # 客户7
]

# 决策变量
# inter[j] = 1 如果设施j被破坏
inter = {}
for j in range(num_facilities):
    inter[j] = binary(f'inter_{j}')

# 中断后的分配变量
x_r = {}
for i in range(num_customers):
    for j in range(num_facilities):
        x_r[(i, j)] = binary(f'x_r_{i}_{j}')

y_r = {}
for j in range(num_facilities):
    y_r[j] = binary(f'y_r_{j}')

# 目标函数: 最大化总加权距离（最坏情况）
total_cost_r = Const(0)
for i in range(num_customers):
    for j in range(num_facilities):
        # 如果设施被中断但原本开门，使用原距离
        # 如果设施被中断且原本不开门，使用中断后距离
        # 简化：使用中断后距离
        total_cost_r = total_cost_r + Const(customer_weights[i] * distance_interdicted[i][j]) * x_r[(i, j)]

problem_r = maximize(total_cost_r)

# 约束: 与P-中位数类似
for i in range(num_customers):
    expr = Const(0)
    for j in range(num_facilities):
        expr = expr + x_r[(i, j)]
    problem_r += constraint(f'assign_r_{i}', expr == Const(1))

for i in range(num_customers):
    for j in range(num_facilities):
        constraint_expr = x_r[(i, j)] - y_r[j] <= Const(0)
        problem_r += constraint(f'serve_r_{i}_{j}', constraint_expr)

# 选择p个设施开门
expr = Const(0)
for j in range(num_facilities):
    expr = expr + y_r[j]
problem_r += constraint('num_facilities_r', expr == Const(p))

# 破坏r个设施（但不开门）
expr = Const(0)
for j in range(num_facilities):
    expr = expr + inter[j]
problem_r += constraint('num_interdiction', expr == Const(r))

print(f'\n中断数量: {r}')
print('\n求解中...')
result_r = solve(problem_r)

print('\n中断问题求解结果:')
print(result_r.summary)

# ============================================================
# 简化版本
# ============================================================

print('\n' + '='*60)
print('简化版本 (使用高级语法糖)')
print('='*60)

print('\n基础P-中位数:')
print('  C = set("C", range(num_customers))')
print('  F = set("F", range(num_facilities))')
print('  ')
print('  x = {(i,j): binary(f"x_{i}_{j}") for i in C for j in F}')
print('  y = {j: binary(f"y_{j}") for j in F}')
print('  ')
print('  objective = minimize(sum((i,j), product(C,F), w[i]*d[i][j]*x[i,j]))')
print('  constraints:')
print('    - sum(j, x[i,j]) == 1, for all i  # 每个客户分配到一个设施')
print('    - x[i,j] <= y[j], for all i,j     # 只能分配给开门设施')
print('    - sum(j, y[j]) == p               # 选择p个设施')
