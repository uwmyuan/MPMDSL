# ORMDSL-CUOPT Build Configuration

## Project Structure

```
ormdsl-cuopt/
├── CMakeLists.txt           # CMake build file
├── Makefile                 # Alternative Make build
├── src/
│   ├── CMakeLists.txt       # Source build configuration
│   ├── CUOPTCodeGenerator.scala
│   ├── IR2CUOPT.scala
│   ├── LPGenerator.scala
│   ├── MILPGenerator.scala
│   ├── RoutingGenerator.scala
│   ├── PythonAPI.scala
│   └── main.scala           # Entry point
└── python/
    ├── cuopt_solver.py       # Python wrapper
    └── setup.py             # Python package config
```

## Build Requirements

- CUDA Toolkit 11.0+
- GCC/Clang with C++17 support
- CMake 3.18+
- Python 3.8+ (for Python API)
- NumPy (for Python API)

## Quick Build

```bash
# Configure and build
mkdir build && cd build
cmake ..
make -j$(nproc)

# Install Python package
cd ../python
pip install -e .
```

## Build Options

| Option | Default | Description |
|--------|---------|-------------|
| CUDA_ARCH | 75 | Target GPU architecture |
| BUILD_CUDA | ON | Build CUDA kernels |
| BUILD_PYTHON | ON | Build Python bindings |
| BUILD_TESTS | ON | Build test suite |

## CMakeLists.txt

```cmake
cmake_minimum_required(VERSION 3.18)
project(ORMDSL-CUOPT VERSION 1.0.0 LANGUAGES CXX CUDA)

# Find CUDA
find_package(CUDA REQUIRED)

# Set CUDA architecture
set(CUDA_ARCH 75)
set(CMAKE_CUDA_ARCHITECTURES ${CUDA_ARCH})

# Compiler flags
set(CMAKE_CXX_STANDARD 17)
set(CMAKE_CUDA_STANDARD 17)
set(CMAKE_CUDA_STANDARD_REQUIRED ON)

# Options
option(BUILD_PYTHON "Build Python bindings" ON)
option(BUILD_TESTS "Build tests" ON)

# Include directories
include_directories(${CMAKE_SOURCE_DIR}/src)

# Build main library
add_library(cuopt_solver SHARED
    src/solver.cu
    src/pdlp_solver.cu
    src/milp_solver.cu
    src/routing_solver.cu
)

target_link_libraries(cuopt_solver
    CUDA::cublas
    CUDA::curand
)

# Python bindings
if(BUILD_PYTHON)
    pybind11_add_module(cuopt_pybind src/pybind11.cpp)
    target_link_libraries(cuopt_pybind cuopt_solver)
endif()

# Tests
if(BUILD_TESTS)
    enable_testing()
    add_subdirectory(tests)
endif()
```

## Running Tests

```bash
cd build
ctest --output-on-failure
```

## Performance Tuning

### GPU Selection
```bash
# Use specific GPU
export CUDA_VISIBLE_DEVICES=0

# Multi-GPU parallel solving
export CUDA_VISIBLE_DEVICES=0,1,2,3
```

### Memory Configuration
```bash
# For large problems, increase memory allocation
export CUDA_HEAP_SIZE=4294967296  # 4GB
```
