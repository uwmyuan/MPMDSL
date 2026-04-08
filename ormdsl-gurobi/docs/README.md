# ORMDSL-Gurobi 用户文档

## 📚 文档目录

| 文档 | 说明 |
|------|------|
| [01-安装指南.md](01-安装指南.md) | 环境配置和依赖安装 |
| [02-快速开始.md](02-快速开始.md) | 5分钟快速入门 |
| [03-API参考.md](03-API参考.md) | 完整的API文档 |
| [04-代码生成器.md](04-代码生成器.md) | 代码生成功能详解 |
| [05-最佳实践.md](05-最佳实践.md) | 性能优化和编码规范 |
| [06-示例手册.md](06-示例手册.md) | 完整示例代码 |
| [07-故障排除.md](07-故障排除.md) | 常见问题和解决方案 |

---

## 🔗 快速链接

- [Gurobi官方网站](https://www.gurobi.com/)
- [Gurobi Python API文档](https://www.gurobi.com/documentation/current/refman/py_python_api.html)
- [ORMDSL项目主页](../README.md)

---

## 📋 前置要求

- Python 3.8+
- Gurobi Optimizer 9.0+
- Scala 2.13+ (仅用于代码生成)

## 🚀 快速开始

```python
import gurobipy as gp
from gurobipy import GRB

# 创建模型
model = gp.Model('my_problem')

# 添加变量
x = model.addVar(vtype=GRB.BINARY, name='x')
y = model.addVar(vtype=GRB.CONTINUOUS, lb=0, ub=10, name='y')

# 设置目标函数
model.setObjective(10*x + 5*y, GRB.MAXIMIZE)

# 添加约束
model.addConstr(x + 2*y <= 15)

# 求解
model.optimize()

# 输出结果
if model.status == GRB.OPTIMAL:
    print(f"Optimal value: {model.objVal}")
```

---

## 📞 获取帮助

- 提交 Issue: [GitHub Issues](../../issues)
- 文档问题: 欢迎提交 PR 完善文档
