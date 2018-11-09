cmake_minimum_required(VERSION 3.10)

project(talta)

add_subdirectory("${PROJECT_SOURCE_DIR}/deps/alta-core")
add_subdirectory("${PROJECT_SOURCE_DIR}/deps/ceetah")

set(CMAKE_CXX_STANDARD 17)
set(CMAKE_CXX_STANDARD_REQUIRED on)
set(CMAKE_CXX_EXTENSIONS off)

add_library(talta
  "${PROJECT_SOURCE_DIR}/src/talta.cpp"
  "${PROJECT_SOURCE_DIR}/src/util.cpp"
)
target_include_directories(talta PUBLIC "${PROJECT_SOURCE_DIR}/include")

target_link_libraries(talta PUBLIC altacore ceetah)
