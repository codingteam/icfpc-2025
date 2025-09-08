cmake_minimum_required(VERSION 3.25)

project(solver
  VERSION 0.0.0
  LANGUAGES Fortran)

option(ENABLE_OPENMP "Enable OpenMP parallelization" OFF)

include(CheckIPOSupported)

set(PN ${PROJECT_NAME})

check_ipo_supported(RESULT IPO_AVAILABLE)
if(ENABLE_OPENMP)
  find_package(OpenMP 3.0 REQUIRED)
endif()

add_subdirectory(src)

add_executable(${PN} ${SOURCES})
if(ENABLE_OPENMP)
  target_link_libraries(${PN} PUBLIC OpenMP::OpenMP_Fortran)
endif()
if(IPO_AVAILABLE)
  set_target_properties(${PN} PROPERTIES INTERPROCEDURAL_OPTIMIZATION TRUE)
endif()
