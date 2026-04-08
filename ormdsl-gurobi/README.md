# ORMDSL-Gurobi 模块

将代数模板转换为Gurobi求解器Python代码的模块。

## 功能

- 将中间表示(IR)转换为Gurobi Python API代码
- 支持线性规划(LP)、整数规划(IP)、混合整数规划(MILP)
- 自动处理集合操作、索引表达式、聚合函数

## 文件结构

```
ormdsl-gurobi/
├── src/
│   ├── IR2Gurobi.scala      # IR到Gurobi代码生成器
│   ├── AST2Gurobi.scala      # AST到Gurobi代码生成器  
│   ├── GurobiModel.scala      # Gurobi模型包装类
│   └── CodeGenerator.scala    # 代码生成器基类
└── examples/                 # 示例问题
    └── knapsack_example.py   # 背包问题Gurobi实现
```

## 使用方法

```scala
import ormgurobi._

// 定义问题
val model = GurobiModel("my_model")
model.setObjective(expression, sense)

// 添加约束
model.addConstraint(name, equation)

// 生成代码
val code = model.generatePythonCode()
model.solve()
```

## Gurobi安装

需要安装Gurobi Optimizer及其Python API:

```bash
pip install gurobipy
```

或从 https://www.gurobi.com/download/ 下载安装。
