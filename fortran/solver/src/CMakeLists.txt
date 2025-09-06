set(dir "${CMAKE_CURRENT_SOURCE_DIR}")

list(APPEND SOURCES
  ${dir}/main.f90
  ${dir}/door.f90
  ${dir}/room.f90
  ${dir}/library.f90
  ${dir}/solver.f90
)

set(SOURCES ${SOURCES} PARENT_SCOPE)
