cmake_minimum_required(VERSION 3.23)
project(playground LANGUAGES CXX)

add_executable(ode
src/main.cpp
src/debug.cpp
src/lexer.cpp
src/parser.cpp
src/compiler.cpp
src/stack_allocator.cpp
src/executor.cpp)

target_compile_options(ode PRIVATE
    $<$<CXX_COMPILER_ID:Clang,GNU>:-march=native>
    # Warning level
    $<$<CXX_COMPILER_ID:Clang>:-Werror> # No GNU here :)
    $<$<CXX_COMPILER_ID:Clang>:-Wall>
    $<$<CXX_COMPILER_ID:Clang,GNU>:-Wextra>
    $<$<CXX_COMPILER_ID:Clang,GNU>:-Wpedantic>
    $<$<CXX_COMPILER_ID:Clang,GNU>:-Wshadow>
    $<$<CXX_COMPILER_ID:Clang,GNU>:-Wconversion>
    $<$<CXX_COMPILER_ID:Clang,GNU>:-Wnon-virtual-dtor>
    $<$<CXX_COMPILER_ID:Clang,GNU>:-Woverloaded-virtual>
    $<$<CXX_COMPILER_ID:Clang,GNU>:-Wpessimizing-move>
    $<$<CXX_COMPILER_ID:Clang,GNU>:-Wredundant-move>

    $<$<CXX_COMPILER_ID:GNU>:-Wno-missing-field-initializers>

    $<$<CXX_COMPILER_ID:MSVC>:/WX>
    $<$<CXX_COMPILER_ID:MSVC>:/W4>
    $<$<CXX_COMPILER_ID:MSVC>:/permissive->
    $<$<CXX_COMPILER_ID:MSVC>:/wd4324> # "structure was padded due to alignment specifier"

    # utils
    $<$<CXX_COMPILER_ID:Clang,GNU>:-std=c++20>
    $<$<CXX_COMPILER_ID:MSVC>:/std:c++latest>
)
