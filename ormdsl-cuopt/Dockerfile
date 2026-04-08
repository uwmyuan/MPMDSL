# ORMDSL-CUOPT Docker GPU 测试环境
# 基于 NVIDIA cuOpt 官方镜像

FROM nvidia/cuopt:25.5.0-cuda12.8-py312

# 设置工作目录
WORKDIR /workspace

# 安装基础依赖
RUN pip install --no-cache-dir \
    pytest \
    pytest-timeout \
    numpy \
    scipy

# 复制源代码
COPY . /workspace/

# 设置环境变量
ENV PYTHONUNBUFFERED=1
ENV CUDA_VISIBLE_DEVICES=0

# 默认入口点
CMD ["pytest", "-v", "--tb=short", "tests/docker_test_gpu.py"]
