# ORMDSL API 参考文档

本文档提供ORMDSL的完整API参考。

---

## 1. 核心类

### 1.1 Model 类

```python
class Model:
    """优化模型主类"""
```

#### 构造函数

```python
Model(name: str)
```

| 参数 | 类型 | 说明 |
|------|------|------|
| name | str | 模型名称 |

#### 方法

##### addVar()

添加单个决策变量。

```python
def addVar(self,
           name: str,
           vtype: str = "continuous",
           lb: float = 0.0,
           ub: float = float('inf'),
           obj: float = 0.0) -> Var
```

| 参数 | 类型 | 默认值 | 说明 |
|------|------|--------|------|
| name | str | - | 变量名 |
| vtype | str | "continuous" | 变量类型: "continuous", "integer", "binary" |
| lb | float | 0.0 | 下界 |
| ub | float | inf | 上界 |
| obj | float | 0.0 | 目标函数系数 |

##### addVars()

添加索引变量。

```python
def addVars(self,
            indices: List[str],
            vtype: str = "continuous",
            lb: float = 0.0,
            ub: float = float('inf')) -> Dict[str, Var]
```

##### setObjective()

设置目标函数。

```python
def setObjective(self,
                 expr: Expr,
                 sense: str = "minimize") -> None
```

| 参数 | 类型 | 说明 |
|------|------|------|
| expr | Expr | 目标表达式 |
| sense | str | "minimize" 或 "maximize" |

##### addConstr()

添加约束。

```python
def addConstr(self,
              name: str,
              constraint: Expr,
              index: str = None) -> Constraint
```

##### solve()

求解模型。

```python
def solve(self,
          solver: str = "gurobi",
          time_limit: float = None,
          mip_gap: float = None) -> SolveResult
```

| 参数 | 类型 | 说明 |
|------|------|------|
| solver | str | 求解器: "gurobi", "cplex", "glpk" |
| time_limit | float | 时间限制（秒） |
| mip_gap | float | 最优性间隙容忍度 |

##### toAMPL()

生成AMPL代码。

```python
def toAMPL(self) -> str
```

##### toGurobi()

生成Gurobi Python代码。

```python
def toGurobi(self) -> str
```

##### toLatex()

生成LaTeX公式。

```python
def toLatex(self) -> str
```

---

### 1.2 Var 类

决策变量类。

```python
class Var:
    """决策变量"""
    
    def __init__(self, name: str, vtype: str, lb: float, ub: float)
    
    name: str      # 变量名
    vtype: str     # 变量类型
    lb: float      # 下界
    ub: float      # 上界
    value: float   # 求解后的值
```

#### 运算符重载

```python
# 算术运算
var + var     # 加法
var - var     # 减法
var * var     # 乘法
var / var     # 除法
var ** n      # 幂运算

# 比较运算
var <= expr   # 小于等于
var >= expr   # 大于等于
var == expr   # 等于
```

---

### 1.3 Expr 类

代数表达式类。

```python
class Expr:
    """代数表达式"""
    
    def __init__(self)
    
    @staticmethod
    def Constant(value: float) -> Expr
    
    @staticmethod
    def Var(v: Var) -> Expr
    
    @staticmethod
    def Sum(terms: List[Expr]) -> Expr
    
    @staticmethod
    def LinExpr(coeffs: List[float], vars: List[Var]) -> Expr
```

---

## 2. 约束相关

### 2.1 Constraint 类

```python
class Constraint:
    """约束基类"""
    
    def __init__(self, name: str, lhs: Expr, sense: str, rhs: Expr)
    
    name: str    # 约束名称
    lhs: Expr    # 左侧表达式
    sense: str   # 关系符: "<=", ">=", "=="
    rhs: Expr    # 右侧表达式
```

### 2.2 RangeConstraint 类

范围约束。

```python
class RangeConstraint:
    """范围约束: lb <= expr <= ub"""
    
    def __init__(self,
                 name: str,
                 expr: Expr,
                 lb: float,
                 ub: float)
```

### 2.3 QualifiedConstraint 类

带索引限定的约束。

```python
class QualifiedConstraint:
    """带限定的约束"""
    
    def __init__(self,
                 name: str,
                 constraint: Constraint,
                 index_set: List[str],
                 filter_func: Callable = None)
```

### 2.4 IndicatorConstraint 类

指示符约束。

```python
class IndicatorConstraint:
    """指示符约束: if x == 1 then expr <= rhs"""
    
    def __init__(self,
                 indicator_var: Var,
                 indicator_value: int,
                 expr: Expr,
                 sense: str,
                 rhs: Expr)
```

---

## 3. 集合操作

### 3.1 Set 类

```python
class Set:
    """索引集合"""
    
    def __init__(self, name: str, elements: List[Any])
    
    name: str           # 集合名称
    elements: List[Any] # 集合元素
    size: int           # 集合大小
    
    def __iter__(self)
    def __len__(self)
    def __contains__(self, item)
```

### 3.2 SetExpr 类

集合表达式。

```python
class SetExpr:
    """集合表达式"""
    
    @staticmethod
    def Union(set1: Set, set2: Set) -> Set
    
    @staticmethod
    def Intersection(set1: Set, set2: Set) -> Set
    
    @staticmethod
    def Difference(set1: Set, set2: Set) -> Set
    
    @staticmethod
    def Cross(set1: Set, set2: Set) -> Set
    
    @staticmethod
    def Filter(set: Set, predicate: Callable) -> Set
```

---

## 4. 聚合函数

### 4.1 Sum 函数

求和聚合。

```python
def Sum(index_set: Set, 
        expr_func: Callable[[Any], Expr]) -> Expr
```

**示例:**

```python
# sum(i in customers, demand[i] * x[i])
Sum(customers, lambda i: demand[i] * x[i])
```

### 4.2 Min/Max 函数

最小/最大聚合。

```python
def Min(index_set: Set, 
        expr_func: Callable[[Any], Expr]) -> Expr

def Max(index_set: Set, 
        expr_func: Callable[[Any], Expr]) -> Expr
```

### 4.3 Count 函数

计数聚合。

```python
def Count(index_set: Set,
          predicate: Callable[[Any], bool]) -> Expr
```

**示例:**

```python
# count(i in I, x[i] == 1)
Count(customers, lambda i: x[i] == 1)
```

### 4.4 Cardinality 函数

集合大小。

```python
def Cardinality(set: Set) -> Expr
```

---

## 5. 约束集类 (CS Class)

### 5.1 CS 基类

```python
class CS:
    """约束集类基类"""
    
    def __init__(self):
        self.variables = []
        self.parameters = []
        self.constraints = []
    
    def get_constraints(self) -> List[Constraint]
    def get_variables(self) -> List[Var]
    def get_parameters(self) -> List[Parameter]
```

### 5.2 使用示例

```python
class Vehicle(CS):
    """船舶约束集类"""
    
    def __init__(self, name: str, length: float, service_time: float):
        super().__init__()
        self.name = name
        self.length = length
        self.service_time = service_time
        
        # 决策变量
        self.arrival = Var(f"{name}_arr", lb=0)
        self.departure = Var(f"{name}_dep", lb=0)
        
        self.variables = [self.arrival, self.departure]
    
    def get_constraints(self) -> List[Constraint]:
        return [
            Constraint(f"{self.name}_service",
                       self.departure - self.arrival,
                       "==",
                       self.service_time)
        ]
```

---

## 6. 结果类

### 6.1 SolveResult 类

求解结果。

```python
class SolveResult:
    """求解结果"""
    
    status: str           # 求解状态: "OPTIMAL", "TIME_LIMIT", etc.
    objective_value: float  # 目标函数值
    runtime: float         # 求解时间（秒）
    mip_gap: float         # 最终最优性间隙
    num_vars: int          # 变量数量
    num_constraints: int    # 约束数量
    
    def is_optimal(self) -> bool
    def is_feasible(self) -> bool
    def get_value(self, var: Var) -> float
    def get_values(self, vars: List[Var]) -> Dict[str, float]
```

---

## 7. 代码生成

### 7.1 AMPL 生成

```python
from ormdsl import Model

model = Model("example")
# ... 定义模型 ...

ampl_code = model.toAMPL()

# 保存到文件
with open("model.run", "w") as f:
    f.write(ampl_code)
```

**生成的AMPL代码格式:**

```ampl
# Model: example
# Generated by ORMDSL

# Decision Variables
var x >= 0, <= 10;

# Objective
minimize obj: 10 * x;

# Constraints
subject to c1: 2 * x <= 15;
```

### 7.2 Gurobi 生成

```python
gurobi_code = model.toGurobi()

# 保存到文件
with open("model.py", "w") as f:
    f.write(gurobi_code)
```

**生成的Gurobi代码格式:**

```python
#!/usr/bin/env python3
import gurobipy as gp
from gurobipy import GRB

def create_model():
    model = gp.Model("example")
    x = model.addVar(lb=0, ub=10, vtype=GRB.CONTINUOUS)
    model.setObjective(10 * x, GRB.MINIMIZE)
    model.addConstr(2 * x <= 15)
    model.optimize()
    return model

if __name__ == "__main__":
    create_model()
```

### 7.3 LaTeX 生成

```python
latex_code = model.toLatex()

# 输出
print(latex_code)
```

**生成的LaTeX代码:**

```latex
\begin{align}
\min \quad & 10x \\
\text{s.t.} \quad & 2x \leq 15 \\
             & x \geq 0
\end{align}
```

---

## 8. 配置参数

### 8.1 模型参数

```python
model.params.time_limit = 3600        # 时间限制（秒）
model.params.mip_gap = 0.001          # 最优性间隙
model.params.threads = 8              # 线程数
model.params.heuristics = 0.5          # 启发式强度
```

### 8.2 求解器参数

```python
model.solver_params["Gurobi"]["Method"] = 2       # 求解方法
model.solver_params["Gurobi"]["Presolve"] = 2     # 预求解级别
model.solver_params["CPLEX"]["TimeLimit"] = 3600  # CPLEX时间限制
```

---

## 9. 异常类

### 9.1 ORMDSLException

基础异常类。

```python
class ORMDSLException(Exception):
    """ORMDSL基础异常"""
    pass
```

### 9.2 派生异常

```python
class ModelError(ORMDSLException):
    """模型定义错误"""
    pass

class ConstraintError(ORMDSLException):
    """约束定义错误"""
    pass

class SolverError(ORMDSLException):
    """求解器错误"""
    pass

class ValidationError(ORMDSLException):
    """验证错误"""
    pass
```

---

## 10. 常量

### 10.1 变量类型

```python
VarType.CONTINUOUS = "continuous"  # 连续变量
VarType.INTEGER = "integer"       # 整数变量
VarType.BINARY = "binary"         # 二进制变量
```

### 10.2 目标方向

```python
Sense.MINIMIZE = "minimize"  # 最小化
Sense.MAXIMIZE = "maximize"  # 最大化
```

### 10.3 求解状态

```python
Status.OPTIMAL = "OPTIMAL"           # 最优解
Status.FEASIBLE = "FEASIBLE"        # 可行解
Status.INFEASIBLE = "INFEASIBLE"    # 不可行
Status.UNBOUNDED = "UNBOUNDED"      # 无界
Status.TIME_LIMIT = "TIME_LIMIT"    # 时间限制
Status.MEMORY_LIMIT = "MEMORY_LIMIT" # 内存限制
```
