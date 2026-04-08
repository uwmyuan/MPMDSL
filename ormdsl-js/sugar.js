/**
 * ORMDSL JavaScript 语法糖模块
 *
 * 提供简洁、直观的 DSL 语法，让优化问题的编写更加自然
 *
 * 使用方法：
 *   import { binary, integer, minimize, constraint, solve } from './sugar.js';
 *
 *   // 变量声明
 *   const x = binary('x', { shape: [n, m] });
 *   const y = integer('y', { lb: 0, ub: 100 });
 *   const z = continuous('z', { lb: 0.0 });
 *
 *   // 问题定义
 *   const problem = minimize(sum(i, I, c[i] * x[i]));
 *   const problem = maximize(sum(i, I, profit[i] * x[i]));
 *
 *   // 约束定义
 *   problem.add(constraint('capacity', sum(i, I, a[i] * x[i]) <= capacity));
 *
 *   // 求解结果
 *   const result = solve(problem);
 *   console.log(result.summary);
 */

// ============================================================
// 一、变量声明语法糖
// ============================================================

class BinaryVar {
    constructor(name, options = {}) {
        this.name = name;
        this.shape = options.shape || [];
        this.lb = 0;
        this.ub = 1;
        this._type = 'binary';
    }

    /** 支持 x[i, j] 索引访问 */
    get(key) {
        if (Array.isArray(key)) {
            key = key.join(',');
        }
        return new IndexedVar(this.name, key);
    }
}

class IntegerVar {
    constructor(name, options = {}) {
        this.name = name;
        this.shape = options.shape || [];
        this.lb = options.lb ?? 0;
        this.ub = options.ub ?? null;
        this._type = 'integer';
    }

    get(key) {
        if (Array.isArray(key)) {
            key = key.join(',');
        }
        return new IndexedVar(this.name, key);
    }
}

class ContinuousVar {
    constructor(name, options = {}) {
        this.name = name;
        this.shape = options.shape || [];
        this.lb = options.lb ?? null;
        this.ub = options.ub ?? null;
        this._type = 'continuous';
    }

    get(key) {
        if (Array.isArray(key)) {
            key = key.join(',');
        }
        return new IndexedVar(this.name, key);
    }
}

class IndexedVar {
    constructor(name, index) {
        this.name = name;
        this.index = index;
    }

    toString() {
        return `${this.name}[${this.index}]`;
    }
}

/**
 * 创建二进制变量
 *
 * 示例：
 *   const x = binary('x');
 *   const y = binary('y', { shape: [n, m] });
 */
function binary(name, options = {}) {
    return new BinaryVar(name, options);
}

/**
 * 创建整数变量
 *
 * 示例：
 *   const y = integer('y', { lb: 0, ub: 100 });
 */
function integer(name, options = {}) {
    return new IntegerVar(name, options);
}

/**
 * 创建连续变量
 *
 * 示例：
 *   const z = continuous('z', { lb: 0.0, ub: 1.0 });
 */
function continuous(name, options = {}) {
    return new ContinuousVar(name, options);
}

// ============================================================
// 二、表达式语法糖
// ============================================================

class Exp {
    /** 加法: x + y */
    add(other) { return new Plus(this, toExpr(other)); }
    plus(other) { return this.add(other); }

    /** 减法: x - y */
    sub(other) { return new Minus(this, toExpr(other)); }
    minus(other) { return this.sub(other); }

    /** 乘法: x * y */
    mul(other) { return new Times(this, toExpr(other)); }
    times(other) { return this.mul(other); }

    /** 除法: x / y */
    div(other) { return new Div(this, toExpr(other)); }
    dividedBy(other) { return this.div(other); }

    /** 幂运算: x^n */
    pow(n) { return new Pow(this, n); }

    /** 绝对值 */
    abs() { return new Abs(this); }

    /** 负值 */
    neg() { return new Neg(this); }

    /** 比较运算符 */
    le(other) { return new Comparison(this, '<=', toExpr(other)); }  // <=
    ge(other) { return new Comparison(this, '>=', toExpr(other)); }  // >=
    eq(other) { return new Comparison(this, '==', toExpr(other)); }  // ==
    lt(other) { return new Comparison(this, '<', toExpr(other)); }   // <
    gt(other) { return new Comparison(this, '>', toExpr(other)); }    // >
}

/** 常量 */
class Const extends Exp {
    constructor(value) {
        super();
        this.value = value;
    }
    toString() { return String(this.value); }
}

/** 加法 */
class Plus extends Exp {
    constructor(left, right) {
        super();
        this.left = toExpr(left);
        this.right = toExpr(right);
    }
    toString() { return `(${this.left} + ${this.right})`; }
}

/** 减法 */
class Minus extends Exp {
    constructor(left, right) {
        super();
        this.left = toExpr(left);
        this.right = toExpr(right);
    }
    toString() { return `(${this.left} - ${this.right})`; }
}

/** 乘法 */
class Times extends Exp {
    constructor(left, right) {
        super();
        this.left = toExpr(left);
        this.right = toExpr(right);
    }
    toString() { return `(${this.left} * ${this.right})`; }
}

/** 除法 */
class Div extends Exp {
    constructor(left, right) {
        super();
        this.left = toExpr(left);
        this.right = toExpr(right);
    }
    toString() { return `(${this.left} / ${this.right})`; }
}

/** 取负 */
class Neg extends Exp {
    constructor(expr) {
        super();
        this.expr = toExpr(expr);
    }
    toString() { return `(-${this.expr})`; }
}

/** 绝对值 */
class Abs extends Exp {
    constructor(expr) {
        super();
        this.expr = toExpr(expr);
    }
    toString() { return `abs(${this.expr})`; }
}

/** 幂运算 */
class Pow extends Exp {
    constructor(base, exponent) {
        super();
        this.base = toExpr(base);
        this.exponent = typeof exponent === 'number' ? new Const(exponent) : exponent;
    }
    toString() { return `(${this.base})^${this.exponent}`; }
}

/** 比较表达式 */
class Comparison {
    constructor(left, op, right) {
        this.left = toExpr(left);
        this.op = op;
        this.right = toExpr(right);
    }
    toString() { return `(${this.left} ${this.op} ${this.right})`; }
}

/** 转换为表达式 */
function toExpr(x) {
    if (x instanceof Exp) return x;
    if (typeof x === 'number') return new Const(x);
    if (typeof x === 'string') return new IndexedVar(x, '');
    return x;
}

// ============================================================
// 三、求和语法糖
// ============================================================

/**
 * 求和表达式
 *
 * 示例：
 *   sum(i, I, c[i] * x[i])
 *   sum((i, j), I, a[i,j] * x[i,j])
 */
class Sum extends Exp {
    constructor(indexVars, indexSet, body) {
        super();
        this.indexVars = Array.isArray(indexVars) ? indexVars : [indexVars];
        this.indexSet = indexSet;
        this.body = body;
    }
    toString() {
        const vars = this.indexVars.join(',');
        return `sum(${vars} in ${this.indexSet}, ${this.body})`;
    }
}

/**
 * 求和语法糖函数
 *
 * 示例：
 *   sum('i', I, c[i] * x[i])
 */
function sum(indexVar, indexSetOrBody, body) {
    if (body === undefined) {
        // sum(i, I) { expr } 形式
        return new Sum(indexVar, null, indexSetOrBody);
    }
    // sum(i, I, expr) 形式
    return new Sum(indexVar, indexSetOrBody, body);
}

// ============================================================
// 四、集合定义语法糖
// ============================================================

/**
 * 创建集合
 *
 * 示例：
 *   const I = set('I', [1, 2, 3, 4, 5]);
 *   const J = set('J', 1, 10);  // {1,2,...,10}
 */
class Set {
    constructor(name, values) {
        this.name = name;
        this.values = Array.isArray(values) ? values : this._range(1, values);
    }

    _range(start, end) {
        const arr = [];
        for (let i = start; i <= end; i++) arr.push(i);
        return arr;
    }

    /** 集合大小 */
    get size() { return this.values.length; }

    /** 迭代器 */
    *[Symbol.iterator]() {
        yield* this.values;
    }
}

function set(name, startOrValues, end) {
    if (end !== undefined) {
        // set('I', 1, 10) => {1,2,...,10}
        const values = [];
        for (let i = startOrValues; i <= end; i++) values.push(i);
        return new Set(name, values);
    }
    return new Set(name, startOrValues);
}

/** 集合并集 */
function union(s1, s2) {
    const values = [...new Set([...s1.values, ...s2.values])];
    return new Set(`${s1.name}_union_${s2.name}`, values);
}

/** 集合交集 */
function intersection(s1, s2) {
    const values = s1.values.filter(v => s2.values.includes(v));
    return new Set(`${s1.name}_intersect_${s2.name}`, values);
}

/** 集合差集 */
function difference(s1, s2) {
    const values = s1.values.filter(v => !s2.values.includes(v));
    return new Set(`${s1.name}_diff_${s2.name}`, values);
}

// ============================================================
// 五、问题定义语法糖
// ============================================================

/**
 * 优化问题
 */
class Problem {
    constructor(objective = null, sense = 'min') {
        this.objective = objective;
        this.sense = sense;  // 'min' or 'max'
        this.constraints = [];
        this.variables = [];
    }

    /** 添加约束 */
    add(constraint) {
        this.constraints.push(constraint);
        return this;
    }

    /** 添加变量 */
    addVar(variable) {
        this.variables.push(variable);
        return this;
    }

    toString() {
        const senseStr = this.sense === 'min' ? 'minimize' : 'maximize';
        return `Problem(${senseStr} ${this.objective})\n  Constraints: ${this.constraints.length}`;
    }
}

/**
 * 创建最小化问题
 *
 * 示例：
 *   const problem = minimize(sum('i', I, c[i] * x[i]));
 */
function minimize(expr) {
    return new Problem(expr, 'min');
}

/**
 * 创建最大化问题
 *
 * 示例：
 *   const problem = maximize(sum('i', I, profit[i] * x[i]));
 */
function maximize(expr) {
    return new Problem(expr, 'max');
}

/**
 * 创建命名约束
 *
 * 示例：
 *   problem.add(constraint('capacity', sum('i', I, a[i] * x[i]) <= 10));
 */
function constraint(name, equation) {
    return new NamedConstraint(name, equation);
}

class NamedConstraint {
    constructor(name, equation) {
        this.name = name;
        this.equation = equation;
    }
    toString() {
        return `[${this.name}] ${this.equation}`;
    }
}

// ============================================================
// 六、结果提取语法糖
// ============================================================

/**
 * 求解结果
 */
class SolverResult {
    constructor(data = {}) {
        this.status = data.status || 'UNKNOWN';
        this.objective = data.objective || 0;
        this.values = data.values || {};
        this.violations = data.violations || [];
    }

    /** 获取变量值 */
    get(name) {
        return this.values[name] ?? 0;
    }

    /** 简写访问 */
    call(name) { return this.get(name); }

    /** 是否可行 */
    get isFeasible() {
        return ['OPTIMAL', 'FEASIBLE'].includes(this.status);
    }

    /** 是否最优 */
    get isOptimal() {
        return this.status === 'OPTIMAL';
    }

    /** 结果摘要 */
    get summary() {
        const varLines = Object.keys(this.values).length === 0
            ? '  (无)'
            : Object.entries(this.values).map(([k, v]) => `  ${k} = ${v}`).join('\n');

        return `═══════════════════════════════════════
 求解状态: ${this.status}
 目标函数值: ${this.objective}
═══════════════════════════════════════
 决策变量:
${varLines}
═══════════════════════════════════════`;
    }
}

/**
 * 求解函数
 *
 * 示例：
 *   const result = solve(problem);
 *   console.log(result.summary);
 *   console.log(result.get('x[1,2]'));
 */
function solve(problem) {
    // TODO: 实际调用求解器
    // 这里返回模拟结果
    return new SolverResult({
        status: 'OPTIMAL',
        objective: 0,
        values: {}
    });
}

// ============================================================
// 七、导出
// ============================================================

export {
    // 变量
    BinaryVar, IntegerVar, ContinuousVar, IndexedVar,
    binary, integer, continuous,
    // 表达式
    Exp, Const, Plus, Minus, Times, Div, Neg, Abs, Pow, Comparison,
    toExpr,
    // 求和
    Sum, sum,
    // 集合
    Set, set, union, intersection, difference,
    // 问题
    Problem, minimize, maximize, constraint, NamedConstraint,
    // 结果
    SolverResult, solve,
};
