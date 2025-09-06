cmake_minimum_required(VERSION 3.25)

project(solver
  VERSION 0.0.0
  LANGUAGES Fortran)

include(CheckIPOSupported)

set(PN ${PROJECT_NAME})

check_ipo_supported(RESULT IPO_AVAILABLE)

add_subdirectory(src)

add_executable(${PN} ${SOURCES})
if(IPO_AVAILABLE)
  set_target_properties(${PN} PROPERTIES INTERPROCEDURAL_OPTIMIZATION TRUE)
endif()
