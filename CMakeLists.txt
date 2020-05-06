cmake_minimum_required(VERSION 3.15)

set (REFERENCE_COUNTER_VERSION "1.0")
set (ALLOWED_BUILD_TYPES "Debug" "Release")

get_property(multi_config GLOBAL PROPERTY GENERATOR_IS_MULTI_CONFIG)
if(multi_config)
  set (CMAKE_CONFIGURATION_TYPES "${ALLOWED_BUILD_TYPES}" CACHE STRING "list of supported configuration types" FORCE)
else()
  set (CMAKE_BUILD_TYPE "Debug" CACHE STRING "Select which configuration to build.")
  set_property (CACHE CMAKE_BUILD_TYPE PROPERTY STRINGS ${ALLOWED_BUILD_TYPES})
endif()

file    (READ ${CMAKE_CURRENT_LIST_DIR}/media/text-art.txt LOGO)
string  (JOIN "\n " GREETING "${LOGO}" "Version: ${REFERENCE_COUNTER_VERSION}\n Build type: ${CMAKE_BUILD_TYPE}\n")
message (STATUS "\n${GREETING}")

project(reference_counter
  VERSION "${REFERENCE_COUNTER_VERSION}"
  LANGUAGES Fortran
  DESCRIPTION "Fortran Reference-Counting Utility"
)

if ("${PROJECT_SOURCE_DIR}" STREQUAL "${PROJECT_BINARY_DIR}")
  message(FATAL_ERROR
    "\n"
    "Error: PROJECT_SOURCE_DIR matches PROJECT_BINARY_DIR: ${PROJECT_BINARY_DIR}\n"
    "This project does not support identical build and source paths.\n"
    "Please delete ${PROJECT_SOURCE_DIR}/CMakeCache.txt and ${PROJECT_SOURCE_DIR}/CMakeFiles/"
    "and then build in a different directory.  This may be accomplished in a bash shell by executing\n"
    "  rm -r CMakeCache.txt CMakeFiles/\n"
    "  mkdir build\n"
    "  cd build\n"
    "  export CC=gcc FC=gfortran\n"
    "  cmake .. \n"
    "or by substituting the appropriate syntax for shells other than bash."
  )
endif()

if(CMAKE_Fortran_COMPILER_ID MATCHES GNU)
  set(CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} -Wall")
endif()

set(CMAKE_Fortran_MODULE_DIRECTORY "${PROJECT_BINARY_DIR}/mod")

foreach(directory src tests)
  add_subdirectory("${directory}")
endforeach()

enable_testing()

function(add_new_test name type)
  set(test_dir "${PROJECT_BINARY_DIR}/tests/${type}/${name}")
  add_test( NAME "${name}"
    COMMAND "${test_dir}/test-${name}"
    WORKING_DIRECTORY "${test_dir}"
  )
  set_tests_properties(${name}
     PROPERTIES PASS_REGULAR_EXPRESSION "Test passed."
  )
endfunction()

foreach(test_name
  count-leaks
)
  add_new_test("${test_name}" "integration")
endforeach()
