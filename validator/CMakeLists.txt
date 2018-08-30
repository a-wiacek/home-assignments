cmake_minimum_required(VERSION 3.6)
project(automata C)

set(CMAKE_C_STANDARD 99)
set(SOURCE_FILES validator.c run.c tester.c helper.h err.h err.c)

add_executable(validator validator.c err.c helper.h)
add_executable(run run.c err.c helper.h)
add_executable(tester tester.c err.c helper.h)
target_link_libraries(validator rt)
target_link_libraries(run rt)
target_link_libraries(tester rt)