cmake_minimum_required(VERSION 3.18)
project(ORMDSL-CUOPT VERSION 1.0.0 LANGUAGES CXX CUDA)

# ============================================================================
# ORMDSL-CUOPT - GPU-accelerated Optimization Solver
# ============================================================================

# Find required packages
find_package(CUDA REQUIRED)
find_package(Python3 COMPONENTS Interpreter Development REQUIRED)

# Set CUDA architecture (adjust for your GPU)
set(CUDA_ARCH "75" CACHE STRING "Target GPU architecture (e.g., 70, 75, 80, 86)")

# Compiler settings
set(CMAKE_CXX_STANDARD 17)
set(CMAKE_CXX_STANDARD_REQUIRED ON)
set(CMAKE_CUDA_STANDARD 17)
set(CMAKE_CUDA_STANDARD_REQUIRED ON)

# Release optimizations
set(CMAKE_CUDA_FLAGS_RELEASE "-O3 --use_fast_math")
set(CMAKE_CXX_FLAGS_RELEASE "-O3 -march=native")

# Build options
option(BUILD_SHARED_LIBS "Build shared libraries" ON)
option(BUILD_PYTHON "Build Python bindings" ON)
option(BUILD_CUDA_KERNELS "Build CUDA kernels" ON)
option(BUILD_TESTS "Build test suite" ON)

# Output directories
set(CMAKE_LIBRARY_OUTPUT_DIRECTORY ${CMAKE_BINARY_DIR}/lib)
set(CMAKE_RUNTIME_OUTPUT_DIRECTORY ${CMAKE_BINARY_DIR}/bin)
set(CMAKE_ARCHIVE_OUTPUT_DIRECTORY ${CMAKE_BINARY_DIR}/lib)

# Include directories
include_directories(
    ${CMAKE_SOURCE_DIR}/include
    ${CUDA_INCLUDE_DIRS}
)

# ============================================================================
# Main CUDA Solver Library
# ============================================================================

if(BUILD_CUDA_KERNELS)
    set(SOLVER_SOURCES
        src/solver.cu
        src/pdlp_solver.cu
        src/milp_solver.cu
        src/routing_solver.cu
        src/common.cu
    )
    
    cuda_add_library(cuopt_solver ${SOLVER_SOURCES})
    
    target_compile_options(cuopt_solver PRIVATE
        $<$<COMPILE_LANGUAGE:CUDA>:
            --expt-relaxed-constexpr
            --use_fast_math
            -Xcompiler=-fPIC
        >
    )
    
    target_link_libraries(cuopt_solver
        PUBLIC
            CUDA::cublas
            CUDA::curand
            pthread
    )
    
    target_include_directories(cuopt_solver PUBLIC
        $<BUILD_INTERFACE:${CMAKE_SOURCE_DIR}/include>
        $<INSTALL_INTERFACE:include>
    )
endif()

# ============================================================================
# Python Bindings (pybind11)
# ============================================================================

if(BUILD_PYTHON)
    find_package(pybind11 CONFIG REQUIRED)
    
    pybind11_add_module(cuopt_pybind
        src/pybind11_binding.cpp
    )
    
    if(BUILD_CUDA_KERNELS)
        target_link_libraries(cuopt_pybind PRIVATE cuopt_solver)
    endif()
    
    target_compile_options(cuopt_pybind PRIVATE
        -O3
        -Wall
    )
    
    # Install Python module
    install(TARGETS cuopt_pybind
        LIBRARY DESTINATION ${Python3_SITEARCH}
    )
endif()

# ============================================================================
# CLI Tool
# ============================================================================

add_executable(cuopt_cli
    src/cuopt_cli.cpp
)

if(BUILD_CUDA_KERNELS)
    target_link_libraries(cuopt_cli cuopt_solver)
endif()

install(TARGETS cuopt_cli
    RUNTIME DESTINATION bin
)

# ============================================================================
# Tests
# ============================================================================

if(BUILD_TESTS)
    enable_testing()
    include(GoogleTest)
    
    add_executable(test_lp tests/test_lp.cpp)
    target_link_libraries(test_lp cuopt_solver GTest::gtest_main)
    gtest_discover_tests(test_lp)
    
    add_executable(test_milp tests/test_milp.cpp)
    target_link_libraries(test_milp cuopt_solver GTest::gtest_main)
    gtest_discover_tests(test_milp)
    
    add_executable(test_vrp tests/test_vrp.cpp)
    target_link_libraries(test_vrp cuopt_solver GTest::gtest_main)
    gtest_discover_tests(test_vrp)
endif()

# ============================================================================
# Installation
# ============================================================================

install(TARGETS cuopt_solver
    LIBRARY DESTINATION lib
    ARCHIVE DESTINATION lib
    INCLUDES DESTINATION include
)

install(DIRECTORY include/
    DESTINATION include
    FILES_MATCHING PATTERN "*.h"
)

# ============================================================================
# Summary
# ============================================================================

message(STATUS "")
message(STATUS "ORMDSL-CUOPT Configuration Summary")
message(STATUS "===================================")
message(STATUS "CUDA Architecture: ${CUDA_ARCH}")
message(STATUS "Build Python:       ${BUILD_PYTHON}")
message(STATUS "Build CUDA Kernels: ${BUILD_CUDA_KERNELS}")
message(STATUS "Build Tests:        ${BUILD_TESTS}")
message(STATUS "Python Package:     ${Python3_SITEARCH}")
message(STATUS "")
