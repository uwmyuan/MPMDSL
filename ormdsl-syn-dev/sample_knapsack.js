/**
 * 背包问题 (Knapsack Problem) - 使用 ORMDSL JavaScript 语法糖
 * 
 * 问题描述：
 * - 给定 n 个物品，每个物品有重量 w 和价值 v
 * - 背包容量为 W
 * - 选择物品使总价值最大，同时总重量不超过容量
 * 
 * 数学模型：
 *   maximize sum(i in items) { v_i * x_i }
 *   s.t.   sum(i in items) { w_i * x_i } <= W
 *          x_i in {0, 1}
 */

import { 
    binary, integer, continuous, 
    minimize, maximize, constraint,
    sum, set, union, intersection, difference,
    solve, Problem, Exp, Const, Times, Plus, Comparison
} from './sugar.js';

// ============================================================
// 1. 数据输入
// ============================================================

// 物品数量
const n = 5;

// 物品重量
const weights = [2, 3, 4, 5, 6];

// 物品价值
const values = [3, 4, 5, 6, 7];

// 背包容量
const W = 10;

// ============================================================
// 2. 变量声明
// ============================================================

// 创建二进制决策变量 x[i] - 是否选择物品 i
const x = [];
for (let i = 0; i < n; i++) {
    x[i] = binary(`x_${i}`);
}

// ============================================================
// 3. 问题定义
// ============================================================

// 目标函数：最大化总价值
const totalValue = sum('i', [0, 1, 2, 3, 4], values.reduce((acc, v, i) => {
    return acc.plus(v.times(x[i]));
}, new Const(0)));

// 简化写法
const objective = values.reduce((acc, v, i) => {
    return acc.plus(v.times(x[i]));
}, new Const(0));

// 创建最大化问题
const problem = maximize(objective);

// 约束：总重量不超过容量
const weightConstraint = weights.reduce((acc, w, i) => {
    return acc.plus(w.times(x[i]));
}, new Const(0));

problem.add(constraint('capacity', weightConstraint.le(W)));

// ============================================================
// 4. 求解
// ============================================================

console.log('背包问题求解');
console.log('='.repeat(40));
console.log(`物品数量: ${n}`);
console.log(`容量: ${W}`);
console.log('物品数据:');
weights.forEach((w, i) => {
    console.log(`  物品${i}: 重量=${w}, 价值=${values[i]}`);
});

console.log('\n问题模型:');
console.log(`  maximize ${objective}`);
console.log(`  s.t.     sum(w[i] * x[i]) <= ${W}`);
console.log(`            x[i] in {0, 1}`);

console.log('\n求解中...');
const result = solve(problem);

console.log('\n求解结果:');
console.log(result.summary);

// 输出选中的物品
console.log('\n选中的物品:');
result.values && Object.entries(result.values).forEach(([name, val]) => {
    if (name.startsWith('x_') && val > 0.5) {
        const idx = parseInt(name.split('_')[1]);
        console.log(`  物品${idx}: 重量=${weights[idx]}, 价值=${values[idx]}`);
    }
});

// ============================================================
// 简化版本（纯语法糖风格）
// ============================================================

console.log('\n' + '='.repeat(40));
console.log('简化版本演示');
console.log('='.repeat(40));

// 使用集合
const items = set('items', 0, n - 1);
const weight = (i) => weights[i];
const value = (i) => values[i];

// 目标函数
const obj2 = minimize ? maximize(sum('i', items, value('i').times(x['i']))) : null;

// 约束
const cap2 = sum('i', items, weight('i').times(x['i'])).le(W);

console.log('使用集合的简洁写法:');
console.log('  objective = maximize sum(i in items, value(i) * x[i])');
console.log('  constraint = sum(i in items, weight(i) * x[i]) <= W');
