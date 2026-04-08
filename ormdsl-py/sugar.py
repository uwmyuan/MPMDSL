# -*-coding:utf-8-*-
"""
ORMDSL Python 语法糖模块

提供简洁、直观的 DSL 语法，让优化问题的编写更加自然

使用方法：
    from sugar import *

    # 变量声明
    x = binary("x", shape=(n, m))
    y = integer("y", shape=(n,), lb=0, ub=100)
    z = continuous("z", lb=0.0)

    # 问题定义
    problem = minimize(sum(i, c[i] * x[i]))
    problem = maximize(sum(i, p[i] * x[i]))

    # 约束定义
    problem += constraint("capacity", sum(i, a[i] * x[i]) <= capacity)

    # 求解结果
    result = solve(problem)
    print(result.summary)
"""

from functools import reduce
from typing import List, Dict, Any, Optional, Union


# ============================================================
# 一、变量声明语法糖
# ============================================================

class Variables:
    """变量声明工厂"""

    @staticmethod
    def binary(name: str, shape: tuple = None, indices: List = None):
        """
        创建二进制变量

        示例：
            x = binary("x")              # 标量
            x = binary("x", shape=(n, m))  # 矩阵
        """
        return BinaryVar(name, shape, indices)

    @staticmethod
    def integer(name: str, shape: tuple = None, lb: float = 0, ub: float = None):
        """
        创建整数变量

        示例：
            y = integer("y", lb=0, ub=100)
        """
        return IntegerVar(name, shape, lb=lb, ub=ub)

    @staticmethod
    def continuous(name: str, shape: tuple = None, lb: float = None, ub: float = None):
        """
        创建连续变量

        示例：
            z = continuous("z", lb=0.0, ub=1.0)
        """
        return ContinuousVar(name, shape, lb=lb, ub=ub)

    @staticmethod
    def parse_indexed(name: str):
        """
        解析索引变量名

        示例：
            base, indices = parse_indexed("x[i,j]")  # => ("x", ["i", "j"])
        """
        import re
        pattern = r'(\w+)\[([^\]]+)\]'
        match = re.match(pattern, name)
        if match:
            return match.group(1), match.group(2).split(',')
        return name, []


class BinaryVar:
    """二进制变量"""
    def __init__(self, name: str, shape: tuple = None, indices: List = None):
        self.name = name
        self.shape = shape or ()
        self.indices = indices
        self.lb = 0
        self.ub = 1

    def __getitem__(self, key):
        """支持 x[i,j] 索引访问"""
        if isinstance(key, tuple):
            idx = ','.join(str(k) for k in key)
        else:
            idx = str(key)
        return IndexedVar(self.name, idx)

    def __str__(self):
        if self.shape:
            return f"binary({self.name}{self.shape})"
        return f"binary({self.name})"

    # 运算符重载
    def __add__(self, other):
        return Plus(self, to_expr(other))
    def __radd__(self, other):
        return Plus(to_expr(other), self)
    def __sub__(self, other):
        return Minus(self, to_expr(other))
    def __rsub__(self, other):
        return Minus(to_expr(other), self)
    def __mul__(self, other):
        return Times(self, to_expr(other))
    def __rmul__(self, other):
        return Times(to_expr(other), self)
    def __truediv__(self, other):
        return Div(self, to_expr(other))
    def __rtruediv__(self, other):
        return Div(to_expr(other), self)
    def __neg__(self):
        return Neg(self)
    def __le__(self, other):
        return Comparison(self, '<=', to_expr(other))
    def __ge__(self, other):
        return Comparison(self, '>=', to_expr(other))
    def __eq__(self, other):
        return Comparison(self, '==', to_expr(other))
    def __lt__(self, other):
        return Comparison(self, '<', to_expr(other))
    def __gt__(self, other):
        return Comparison(self, '>', to_expr(other))


class IntegerVar:
    """整数变量"""
    def __init__(self, name: str, shape: tuple = None, lb: float = 0, ub: float = None):
        self.name = name
        self.shape = shape or ()
        self.lb = lb
        self.ub = ub

    def __getitem__(self, key):
        """支持 y[i] 索引访问"""
        if isinstance(key, tuple):
            idx = ','.join(str(k) for k in key)
        else:
            idx = str(key)
        return IndexedVar(self.name, idx)

    # 运算符重载
    def __add__(self, other):
        return Plus(self, to_expr(other))
    def __radd__(self, other):
        return Plus(to_expr(other), self)
    def __sub__(self, other):
        return Minus(self, to_expr(other))
    def __rsub__(self, other):
        return Minus(to_expr(other), self)
    def __mul__(self, other):
        return Times(self, to_expr(other))
    def __rmul__(self, other):
        return Times(to_expr(other), self)
    def __truediv__(self, other):
        return Div(self, to_expr(other))
    def __rtruediv__(self, other):
        return Div(to_expr(other), self)
    def __neg__(self):
        return Neg(self)
    def __le__(self, other):
        return Comparison(self, '<=', to_expr(other))
    def __ge__(self, other):
        return Comparison(self, '>=', to_expr(other))
    def __eq__(self, other):
        return Comparison(self, '==', to_expr(other))
    def __lt__(self, other):
        return Comparison(self, '<', to_expr(other))
    def __gt__(self, other):
        return Comparison(self, '>', to_expr(other))


class ContinuousVar:
    """连续变量"""
    def __init__(self, name: str, shape: tuple = None, lb: float = None, ub: float = None):
        self.name = name
        self.shape = shape or ()
        self.lb = lb
        self.ub = ub

    def __getitem__(self, key):
        """支持 z[i] 索引访问"""
        if isinstance(key, tuple):
            idx = ','.join(str(k) for k in key)
        else:
            idx = str(key)
        return IndexedVar(self.name, idx)

    # 运算符重载
    def __add__(self, other):
        return Plus(self, to_expr(other))
    def __radd__(self, other):
        return Plus(to_expr(other), self)
    def __sub__(self, other):
        return Minus(self, to_expr(other))
    def __rsub__(self, other):
        return Minus(to_expr(other), self)
    def __mul__(self, other):
        return Times(self, to_expr(other))
    def __rmul__(self, other):
        return Times(to_expr(other), self)
    def __truediv__(self, other):
        return Div(self, to_expr(other))
    def __rtruediv__(self, other):
        return Div(to_expr(other), self)
    def __neg__(self):
        return Neg(self)
    def __le__(self, other):
        return Comparison(self, '<=', to_expr(other))
    def __ge__(self, other):
        return Comparison(self, '>=', to_expr(other))
    def __eq__(self, other):
        return Comparison(self, '==', to_expr(other))
    def __lt__(self, other):
        return Comparison(self, '<', to_expr(other))
    def __gt__(self, other):
        return Comparison(self, '>', to_expr(other))


class IndexedVar:
    """索引变量"""
    def __init__(self, name: str, index: str):
        self.name = name
        self.index = index

    def __str__(self):
        return f"{self.name}[{self.index}]"

    # 运算符重载
    def __add__(self, other):
        return Plus(self, to_expr(other))

    def __radd__(self, other):
        return Plus(to_expr(other), self)

    def __mul__(self, other):
        return Times(self, to_expr(other))

    def __rmul__(self, other):
        return Times(to_expr(other), self)

    def __sub__(self, other):
        return Minus(self, to_expr(other))

    def __rsub__(self, other):
        return Minus(to_expr(other), self)

    def __truediv__(self, other):
        return Div(self, to_expr(other))

    def __rtruediv__(self, other):
        return Div(to_expr(other), self)

    # 比较运算符
    def __le__(self, other):
        return Comparison(self, '<=', to_expr(other))

    def __ge__(self, other):
        return Comparison(self, '>=', to_expr(other))

    def __eq__(self, other):
        return Comparison(self, '==', to_expr(other))


def to_expr(x):
    """转换为表达式"""
    if isinstance(x, (int, float)):
        return Const(x)
    return x


# ============================================================
# 二、表达式语法糖
# ============================================================

class Exp:
    """表达式基类"""
    def __add__(self, other):
        return Plus(self, to_expr(other))

    def __radd__(self, other):
        return Plus(to_expr(other), self)

    def __mul__(self, other):
        return Times(self, to_expr(other))

    def __rmul__(self, other):
        return Times(to_expr(other), self)

    def __sub__(self, other):
        return Minus(self, to_expr(other))

    def __rsub__(self, other):
        return Minus(to_expr(other), self)

    def __truediv__(self, other):
        return Div(self, to_expr(other))

    def __rtruediv__(self, other):
        return Div(to_expr(other), self)

    def __neg__(self):
        return Neg(self)

    # 比较运算符
    def __le__(self, other):
        return Comparison(self, '<=', to_expr(other))

    def __ge__(self, other):
        return Comparison(self, '>=', to_expr(other))

    def __eq__(self, other):
        return Comparison(self, '==', to_expr(other))

    def __lt__(self, other):
        return Comparison(self, '<', to_expr(other))

    def __gt__(self, other):
        return Comparison(self, '>', to_expr(other))


class Const(Exp):
    """常量"""
    def __init__(self, value: float):
        self.value = value

    def __str__(self):
        return str(self.value)


class Plus(Exp):
    """加法"""
    def __init__(self, left, right):
        self.left = to_expr(left)
        self.right = to_expr(right)

    def __str__(self):
        return f"({self.left} + {self.right})"


class Minus(Exp):
    """减法"""
    def __init__(self, left, right):
        self.left = to_expr(left)
        self.right = to_expr(right)

    def __str__(self):
        return f"({self.left} - {self.right})"


class Times(Exp):
    """乘法"""
    def __init__(self, left, right):
        self.left = to_expr(left)
        self.right = to_expr(right)

    def __str__(self):
        return f"({self.left} * {self.right})"


class Div(Exp):
    """除法"""
    def __init__(self, left, right):
        self.left = to_expr(left)
        self.right = to_expr(right)

    def __str__(self):
        return f"({self.left} / {self.right})"


class Neg(Exp):
    """取负"""
    def __init__(self, expr):
        self.expr = to_expr(expr)

    def __str__(self):
        return f"(-{self.expr})"


class Pow(Exp):
    """幂运算"""
    def __init__(self, base, exponent):
        self.base = to_expr(base)
        self.exponent = exponent if isinstance(exponent, Exp) else Const(exponent)

    def __str__(self):
        return f"({self.base})^{self.exponent}"


class Comparison:
    """比较表达式"""
    def __init__(self, left, op: str, right):
        self.left = to_expr(left)
        self.op = op
        self.right = to_expr(right)

    def __str__(self):
        return f"({self.left} {self.op} {self.right})"


# ============================================================
# 三、求和语法糖
# ============================================================

class Sum:
    """求和表达式"""

    def __init__(self, index_var, index_set, body):
        """
        示例：
            sum(i, range(n), c[i] * x[i])
            sum(i in I, c[i] * x[i])
        """
        self.index_var = index_var
        self.index_set = index_set
        self.body = body

    def __str__(self):
        return f"sum({self.index_var} in {self.index_set}, {self.body})"

    def __add__(self, other):
        return Plus(self, to_expr(other))

    def __radd__(self, other):
        return Plus(to_expr(other), self)

    def __mul__(self, other):
        return Times(self, to_expr(other))

    def __rmul__(self, other):
        return Times(to_expr(other), self)


def sum(index_var, index_set_or_body, body=None):
    """
    求和语法糖

    示例：
        sum(i, range(n), c[i] * x[i])
        sum(i, I, c[i] * x[i])
    """
    if body is None:
        # sum(i, I) { expr } 形式，index_set_or_body 是 body
        return Sum(index_var, None, index_set_or_body)
    else:
        # sum(i, range(n), expr) 形式
        return Sum(index_var, index_set_or_body, body)


# ============================================================
# 四、集合定义语法糖
# ============================================================

class Set:
    """集合定义"""

    def __init__(self, name: str, values: List):
        self.name = name
        self.values = values

    def __iter__(self):
        return iter(self.values)

    def __len__(self):
        return len(self.values)

    def __str__(self):
        return f"set({self.name}, [{len(self.values)} elements])"


def set(name: str, values: List) -> Set:
    """
    创建集合

    示例：
        I = set("I", range(1, 11))
        J = set("J", [1, 2, 3, 4, 5])
    """
    return Set(name, values)


def range_set(start, end):
    """创建范围集合"""
    return list(range(start, end + 1))


# 集合运算
def union(s1: Set, s2: Set) -> Set:
    """集合并集"""
    return Set(f"{s1.name}_union_{s2.name}", s1.values + [v for v in s2.values if v not in s1.values])


def intersection(s1: Set, s2: Set) -> Set:
    """集合交集"""
    common = [v for v in s1.values if v in s2.values]
    return Set(f"{s1.name}_intersect_{s2.name}", common)


def difference(s1: Set, s2: Set) -> Set:
    """集合差集"""
    diff = [v for v in s1.values if v not in s2.values]
    return Set(f"{s1.name}_diff_{s2.name}", diff)


# ============================================================
# 五、问题定义语法糖
# ============================================================

class Problem:
    """优化问题"""

    def __init__(self, objective=None, sense='min'):
        self.objective = objective
        self.sense = sense  # 'min' or 'max'
        self.constraints = []

    def __iadd__(self, constraint):
        """支持 problem += constraint(...) 语法"""
        if isinstance(constraint, NamedConstraint):
            self.constraints.append(constraint)
        elif isinstance(constraint, Comparison):
            # 直接的比较表达式自动包装为约束
            self.constraints.append(NamedConstraint(f'c{len(self.constraints)+1}', constraint))
        elif callable(constraint):
            # 如果是函数，先执行
            result = constraint()
            if isinstance(result, (NamedConstraint, Comparison)):
                self.constraints.append(result)
        return self

    def add(self, constraint):
        """添加约束"""
        self.constraints.append(constraint)
        return self

    def __str__(self):
        sense_str = "minimize" if self.sense == 'min' else "maximize"
        parts = [f"Problem({sense_str} {self.objective})"]
        if self.constraints:
            parts.append(f"  Constraints: {len(self.constraints)}")
        return '\n'.join(part for part in parts if part)


def minimize(expr) -> Problem:
    """
    创建最小化问题

    示例：
        problem = minimize(sum(i, c[i] * x[i]))
    """
    return Problem(objective=expr, sense='min')


def maximize(expr) -> Problem:
    """
    创建最大化问题

    示例：
        problem = maximize(sum(i, profit[i] * x[i]))
    """
    return Problem(objective=expr, sense='max')


def constraint(name: str, eq) -> 'NamedConstraint':
    """
    创建命名约束

    示例：
        problem += constraint("capacity", sum(i, a[i] * x[i]) <= 10)
    """
    return NamedConstraint(name, eq)


class NamedConstraint:
    """命名约束"""
    def __init__(self, name: str, equation):
        self.name = name
        self.equation = equation

    def __str__(self):
        return f"[{self.name}] {self.equation}"


# ============================================================
# 六、结果提取语法糖
# ============================================================

class SolverResult:
    """求解结果"""

    def __init__(self, status: str, objective: float, values: Dict[str, float], violations: List = None):
        self.status = status
        self.objective = objective
        self.values = values
        self.violations = violations or []

    def __call__(self, name: str) -> float:
        """获取变量值"""
        return self.values.get(name, 0.0)

    @property
    def is_feasible(self) -> bool:
        """是否可行"""
        return self.status in ('OPTIMAL', 'FEASIBLE')

    @property
    def is_optimal(self) -> bool:
        """是否最优"""
        return self.status == 'OPTIMAL'

    @property
    def summary(self) -> str:
        """结果摘要"""
        return f"""
═══════════════════════════════════════
 求解状态: {self.status}
 目标函数值: {self.objective}
═══════════════════════════════════════
 决策变量:
{self._format_values()}
═══════════════════════════════════════
        """.strip()

    def _format_values(self):
        if not self.values:
            return "  (无)"
        return '\n'.join(f"  {k} = {v}" for k, v in self.values.items())


def solve(problem: Problem) -> SolverResult:
    """
    求解函数

    示例：
        result = solve(problem)
        print(result.summary)
        print(result("x[1,2]"))
    """
    # TODO: 实际调用求解器
    # 这里返回模拟结果
    return SolverResult(
        status='OPTIMAL',
        objective=0.0,
        values={}
    )


# ============================================================
# 七、便捷导入
# ============================================================

# 变量声明快捷函数
binary = Variables.binary
integer = Variables.integer
continuous = Variables.continuous

__all__ = [
    # 变量
    'binary', 'integer', 'continuous', 'BinaryVar', 'IntegerVar', 'ContinuousVar',
    # 表达式
    'Exp', 'Const', 'Plus', 'Minus', 'Times', 'Div', 'Neg', 'Pow', 'Comparison',
    # 求和
    'sum', 'Sum',
    # 集合
    'set', 'Set', 'union', 'intersection', 'difference', 'range_set',
    # 问题
    'Problem', 'minimize', 'maximize', 'constraint', 'NamedConstraint',
    # 结果
    'SolverResult', 'solve',
]
