cmake_minimum_required(VERSION 3.23)
project(playground LANGUAGES CXX)

# Executable size with CRT
# Debug 170KB
# Release 48KB
# MinSizeRel 43KB
# RelWithDebInfo 91KB

# Executable with clang freestanding
# clang++.exe src/main.c -o build\ode_size.exe -Os -DUNITY_BUILD -nostdlib -ffreestanding -fuse-ld=lld -fno-stack-check -fno-stack-protector -mno-stack-arg-probe -std=c++20 -lkernel32
# Debug -O0   60KB
# Release -O2 37B
# Size -Os    25KB

# Disable CRT (GS- disable stack checks, Gs<large size> disable chkstk, /stack set size to reserve and commit)
set(CMAKE_EXE_LINKER_FLAGS "/nodefaultlib /stack:0x200000,200000")
set(CMAKE_CXX_FLAGS_DEBUG  "")
set(CMAKE_CXX_FLAGS_RELEASE  "")
set(CMAKE_CXX_FLAGS "/GS- /Gs1000000000")

add_executable(ode
src/main.c
src/debug.c
src/lexer.c
src/parser.c
src/compiler.c
src/stack_allocator.c
src/vm.c
src/executor.c)

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
