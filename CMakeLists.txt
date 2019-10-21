cmake_minimum_required(VERSION 3.10)

project(talta)

add_subdirectory("${PROJECT_SOURCE_DIR}/deps/alta-core")
add_subdirectory("${PROJECT_SOURCE_DIR}/deps/ceetah")

add_library(talta
  "${PROJECT_SOURCE_DIR}/src/talta.cpp"
  "${PROJECT_SOURCE_DIR}/src/util.cpp"
)
target_include_directories(talta PUBLIC "${PROJECT_SOURCE_DIR}/include" "${PROJECT_SOURCE_DIR}/deps/PicoSHA2")

# credit to Austin Lasher (https://medium.com/@alasher/colored-c-compiler-output-with-ninja-clang-gcc-10bfe7f2b949)
option(FORCE_COLORED_OUTPUT "Always produce ANSI-colored output (GNU/Clang only)." TRUE)
if(${FORCE_COLORED_OUTPUT})
  if("${CMAKE_CXX_COMPILER_ID}" STREQUAL "GNU")
    target_compile_options(talta PRIVATE -fdiagnostics-color=always)
  elseif("${CMAKE_CXX_COMPILER_ID}" STREQUAL "Clang" OR "${CMAKE_CXX_COMPILER_ID}" STREQUAL "AppleClang")
    target_compile_options(talta PRIVATE -fcolor-diagnostics)
  endif()
endif()

target_link_libraries(talta PUBLIC altacore ceetah)

set_target_properties(talta
  PROPERTIES
  CXX_STANDARD 17
  CXX_STANDARD_REQUIRED on
  CXX_EXTENSIONS off
  ARCHIVE_OUTPUT_DIRECTORY "${CMAKE_BINARY_DIR}/lib"
  LIBRARY_OUTPUT_DIRECTORY "${CMAKE_BINARY_DIR}/lib"
  RUNTIME_OUTPUT_DIRECTORY "${CMAKE_BINARY_DIR}/bin"
)

# install files
install(TARGETS talta
  ARCHIVE DESTINATION lib
)

install(DIRECTORY "${PROJECT_SOURCE_DIR}/include/"
  DESTINATION include
  FILES_MATCHING PATTERN "*"
)
