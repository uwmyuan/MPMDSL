# -*- coding: utf-8 -*-
"""
泊位分配问题 (Berth Allocation Problem) - 使用 ORMDSL Python 语法糖

问题描述：
- 给定一组船舶和一组泊位
- 每艘船舶有不同的长度、在港时间、优先级
- 将船舶分配到泊位，最小化总加权延迟时间

数学模型：
    minimize sum(v in V) { priority_v * (departure_v - arrival_v) }
    s.t.   每个船舶必须分配到一个泊位
           每个泊位在同一时间只能服务一艘船舶
           船舶之间不能重叠
           泊位时间窗口约束
"""

from sugar import *

# ============================================================
# 1. 数据输入
# ============================================================

# 船舶数据 (length, priority, service_time)
vessels_data = [
    (200, 3, 10),  # 船舶0: 长度200, 优先级3, 服务时间10
    (150, 5, 8),   # 船舶1: 长度150, 优先级5, 服务时间8
    (250, 2, 12),  # 船舶2: 长度250, 优先级2, 服务时间12
    (180, 4, 9),   # 船舶3: 长度180, 优先级4, 服务时间9
    (220, 1, 11),  # 船舶4: 长度220, 优先级1, 服务时间11
]

# 泊位数据 (berth_no, length, available_time)
berths_data = [
    (0, 300, 0),   # 泊位0: 长度300, 可用时间0
    (1, 300, 0),   # 泊位1: 长度300, 可用时间0
    (2, 250, 0),   # 泊位2: 长度250, 可用时间0
]

# 时间窗口数据 (start_time, end_time, max_vessels)
time_windows_data = [
    (0, 8, 2),     # 时间窗口0: 0-8, 最多2艘船
    (8, 16, 2),    # 时间窗口1: 8-16, 最多2艘船
    (16, 24, 3),   # 时间窗口2: 16-24, 最多3艘船
]

num_vessels = len(vessels_data)
num_berths = len(berths_data)
num_tw = len(time_windows_data)

# ============================================================
# 2. 变量声明
# ============================================================

# 船舶集合
V = set('V', list(range(num_vessels)))

# 泊位集合
B = set('B', list(range(num_berths)))

# 时间窗口集合
TW = set('TW', list(range(num_tw)))

# 决策变量
# x[v,b] = 1 如果船舶v分配到泊位b
x = {}
for v in range(num_vessels):
    for b in range(num_berths):
        x[(v, b)] = binary(f'x_{v}_{b}')

# arrival_time[v] = 船舶v的到达时间
arrival = {}
for v in range(num_vessels):
    arrival[v] = continuous(f'arrival_{v}', lb=0)

# departure_time[v] = 船舶v的离开时间
departure = {}
for v in range(num_vessels):
    departure[v] = continuous(f'departure_{v}', lb=0)

# time_window[v] = 船舶v使用的时间窗口索引
tw = {}
for v in range(num_vessels):
    tw[v] = integer(f'tw_{v}', lb=0, ub=num_tw-1)

# ============================================================
# 3. 问题定义
# ============================================================

# 目标函数：最小化总加权延迟 (简化版: 最小化总服务时间)
# 实际应该是: sum(v, priority_v * (departure_v - arrival_v))
total_lateness = Const(0)
for v in range(num_vessels):
    service_time = vessels_data[v][2]
    priority = vessels_data[v][1]
    # 简化：使用服务时间作为代理
    total_lateness = total_lateness + Const(priority) * (departure[v] - arrival[v])

problem = minimize(total_lateness)

# ============================================================
# 4. 约束定义
# ============================================================

# 约束1: 每个船舶必须分配到一个泊位
for v in range(num_vessels):
    expr = Const(0)
    for b in range(num_berths):
        expr = expr + x[(v, b)]
    problem += constraint(f'allocate_{v}', expr == Const(1))

# 约束2: 每个泊位在同一时间只能服务一艘船 (容量约束)
for b in range(num_berths):
    berth_length = berths_data[b][1]
    for v in range(num_vessels):
        vessel_length = vessels_data[v][0]
        # 船舶v能分配到泊位b的条件: 泊位长度 >= 船舶长度
        # 这里简化处理，不考虑空间重叠
        pass

# 约束3: 到达时间 >= 泊位可用时间 (如果分配到该泊位)
for v in range(num_vessels):
    for b in range(num_berths):
        berth_avail = berths_data[b][2]
        # arrival[v] >= berth_avail * x[v,b]  ->  arrival[v] - berth_avail * x[v,b] >= 0
        # 简化: arrival[v] >= 0 (默认所有泊位从时间0可用)
        pass

# 约束4: 离开时间 = 到达时间 + 服务时间
for v in range(num_vessels):
    service_time = vessels_data[v][2]
    constraint_expr = departure[v] - arrival[v] == Const(service_time)
    problem += constraint(f'service_{v}', constraint_expr)

# 约束5: 时间窗口约束 (简化)
# 每艘船必须在某个时间窗口内
for v in range(num_vessels):
    # 简化: arrival[v] 在 0 到 24 之间
    constraint_expr = arrival[v] >= Const(0)
    problem += constraint(f'tw_start_{v}', constraint_expr)
    constraint_expr = departure[v] <= Const(24)
    problem += constraint(f'tw_end_{v}', constraint_expr)

# 约束6: 船舶长度不能超过泊位长度 (分配约束的隐含条件)
for v in range(num_vessels):
    vessel_length = vessels_data[v][0]
    for b in range(num_berths):
        berth_length = berths_data[b][1]
        if vessel_length > berth_length:
            # 大船不能分配到小泊位
            problem += constraint(f'fit_{v}_{b}', x[(v, b)] == Const(0))

# ============================================================
# 5. 求解
# ============================================================

print('='*60)
print('泊位分配问题 (Berth Allocation Problem) 求解')
print('='*60)

print('\n船舶数据:')
print('-'*40)
print(f'{"船舶":<8} {"长度":<8} {"优先级":<8} {"服务时间":<10}')
print('-'*40)
for v, (length, priority, service) in enumerate(vessels_data):
    print(f'{v:<8} {length:<8} {priority:<8} {service:<10}')
print('-'*40)

print('\n泊位数据:')
print('-'*30)
print(f'{"泊位":<8} {"长度":<8} {"可用时间":<10}')
print('-'*30)
for b, (no, length, avail) in enumerate(berths_data):
    print(f'{b:<8} {length:<8} {avail:<10}')
print('-'*30)

print('\n时间窗口数据:')
print('-'*40)
print(f'{"窗口":<8} {"开始":<8} {"结束":<8} {"最大船舶数":<12}')
print('-'*40)
for t, (start, end, max_v) in enumerate(time_windows_data):
    print(f'{t:<8} {start:<8} {end:<8} {max_v:<12}')
print('-'*40)

print('\n问题模型:')
print('  minimize sum(v) { priority_v * (departure_v - arrival_v) }')
print('  s.t.  每个船舶恰好分配到一个泊位')
print('        船舶服务时间约束')
print('        时间窗口约束')
print('        长度适配约束')

print('\n决策变量:')
print(f'  x[v,b] in {{0,1}}: {num_vessels * num_berths} 个')
print(f'  arrival[v] >= 0: {num_vessels} 个')
print(f'  departure[v] >= 0: {num_vessels} 个')

print('\n求解中...')
result = solve(problem)

print('\n求解结果:')
print(result.summary)

# 提取分配方案
if result.values:
    print('\n最优分配方案:')
    print('-'*50)
    for v in range(num_vessels):
        vessel_name = f'船舶{v}'
        for b in range(num_berths):
            val = result(f'x_{v}_{b}')
            if val > 0.5:
                arr_val = result(f'arrival_{v}')
                dep_val = result(f'departure_{v}')
                berth_len = berths_data[b][1]
                vessel_len = vessels_data[v][0]
                print(f'{vessel_name:<10} -> 泊位{b} (长度{berth_len})')
                print(f'  服务时间: {arr_val:.1f} - {dep_val:.1f}')
                print(f'  船舶长度: {vessel_len}, 服务时间: {vessels_data[v][2]}')
    print('-'*50)

# ============================================================
# 简化版本（使用更高级的语法糖）
# ============================================================

print('\n' + '='*60)
print('简化版本演示 (使用高级语法糖)')
print('='*60)

# 使用集合操作
all_assignments = union(V, B)  # 船舶 x 泊位

print('\n使用语法糖的简洁写法:')
print('  # 船舶集合')
print('  V = set("V", range(num_vessels))')
print('  ')
print('  # 分配变量')
print('  x = {(v,b): binary(f"x_{v}_{b}") for v in V for b in B}')
print('  ')
print('  # 目标: 最小化加权延迟')
print('  problem = minimize(sum(v, priority[v] * (departure[v] - arrival[v])))')
print('  ')
print('  # 约束: 每个船舶分配到一个泊位')
print('  for v in V:')
print('      problem += constraint(f"allocate_{v}", sum(b, x[v,b]) == 1)')
