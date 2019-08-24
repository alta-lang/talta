cmake_minimum_required(VERSION 3.10)

project(talta)

# don't treat warnings as errors
# why is this even default behavior in the first place?
# cmake, stop. just stop.
string(REPLACE " -Werror" "" CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS}")
string(REPLACE " /WX" "" CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS}")
string(REPLACE " -Werror" "" CMAKE_C_FLAGS "${CMAKE_C_FLAGS}")
string(REPLACE " /WX" "" CMAKE_C_FLAGS "${CMAKE_C_FLAGS}")

add_subdirectory("${PROJECT_SOURCE_DIR}/deps/alta-core")
add_subdirectory("${PROJECT_SOURCE_DIR}/deps/ceetah")

add_library(talta
  "${PROJECT_SOURCE_DIR}/src/talta.cpp"
  "${PROJECT_SOURCE_DIR}/src/util.cpp"
)
target_include_directories(talta PUBLIC "${PROJECT_SOURCE_DIR}/include")

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
