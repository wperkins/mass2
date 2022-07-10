# -------------------------------------------------------------
# file: CMakeLists.txt
# -------------------------------------------------------------
# -------------------------------------------------------------
# Copyright (c) 2017 Battelle Memorial Institute
# Licensed under modified BSD License. A copy of this license can be
# found in the LICENSE file in the top level directory of this
# distribution.
# -------------------------------------------------------------
# -------------------------------------------------------------
# Created July  9, 2022 by Bill Perkins
# Last Change: 2022-07-10 13:53:58 perk
# -------------------------------------------------------------

cmake_minimum_required(VERSION 3.16)

project(MASS2)
enable_language(Fortran)

list (APPEND CMAKE_MODULE_PATH "${MASS2_SOURCE_DIR}/cmake")

# -------------------------------------------------------------
# compiler options
# -------------------------------------------------------------
if(CMAKE_Fortran_COMPILER_ID MATCHES "GNU")
  # trap floating point exceptions and use preprocessor
  set(CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} -Wall -ffpe-trap=invalid,zero,overflow -cpp -fbacktrace -Wno-unused-dummy-argument")
elseif (CMAKE_Fortran_COMPILER_ID MATCHES "Intel")
  # w/ Intel, build static binaries that trap floating point exceptions
  # and turn on the preprocessor for all files
  set(CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} -fpe0 -fpp -traceback")
endif()

# -------------------------------------------------------------
# MPI compiler
# -------------------------------------------------------------
message(STATUS "Checking MPI ...")
find_package (MPI REQUIRED)
message(STATUS "MPI_Fortran_LIBRARIES: ${MPI_Fortran_LIBRARIES}")

# -------------------------------------------------------------
# Global Arrays
# -------------------------------------------------------------
message(STATUS "Checking Global Arrays ...")

find_package(GA REQUIRED)
include_directories (BEFORE ${GA_INCLUDE_DIRS})
message(STATUS "GA_INCLUDE_DIRS=${GA_INCLUDE_DIRS}")
message(STATUS "GA_LIBRARIES=${GA_LIBRARIES}")

# -------------------------------------------------------------
# PETSc
# -------------------------------------------------------------
message(STATUS "Checking PETSc ...")
include(FindPkgConfig)
set(ENV{PKG_CONFIG_PATH}
  ${PETSC_DIR}/lib/pkgconfig:${PETSC_DIR}/${PETSC_ARCH}/lib/pkgconfig)
pkg_check_modules(PETSc REQUIRED PETSc)
message(STATUS "Using PETSc version ${PETSc_VERSION}")
message(STATUS "PETSc_INCLUDES: ${PETSc_INCLUDE_DIRS}")
message(STATUS "PETSc_LIBRARIES: ${PETSc_LIBRARIES}")

include_directories (BEFORE ${PETSc_INCLUDE_DIRS})

# -------------------------------------------------------------
# CGNS
# -------------------------------------------------------------
message(STATUS "Checking CGNS ...")
find_package(CGNS REQUIRED)
message(STATUS "CGNS_INCLUDE_DIRS: ${CGNS_INCLUDE_DIRS}")
message(STATUS "CGNS_LIBRARIES: ${CGNS_LIBRARIES}")

include_directories (BEFORE ${CGNS_INCLUDE_DIRS})

# -------------------------------------------------------------
# GIT - for banner revision
# -------------------------------------------------------------
find_package(Git)

if (GIT_EXECUTABLE) 

  execute_process(
    COMMAND git rev-parse --abbrev-ref HEAD
    WORKING_DIRECTORY ${CMAKE_SOURCE_DIR}
    OUTPUT_VARIABLE MASS2_BRANCH
    OUTPUT_STRIP_TRAILING_WHITESPACE
  )

  execute_process(
    COMMAND ${GIT_EXECUTABLE} describe --always --abbrev=6 --dirty
    WORKING_DIRECTORY "${MASS2_SOURCE_DIR}"
    OUTPUT_VARIABLE MASS2_REVISION
    OUTPUT_STRIP_TRAILING_WHITESPACE
    )
  execute_process(
    COMMAND ${GIT_EXECUTABLE} log -1 --format=%cd --date=local
    WORKING_DIRECTORY "${MASS2_SOURCE_DIR}"
    OUTPUT_VARIABLE MASS2_REVISION_DATE
    OUTPUT_STRIP_TRAILING_WHITESPACE
    )
else(GIT_EXECUTABLE)
  set(MASS2_BRANCH "Unknown")
  set(MASS2_REVISION "Unknown")
  set(MASS2_REVISION_DATE "Unknown")
endif(GIT_EXECUTABLE)
configure_file(banner.f90.in banner.f90 @ONLY)

# -------------------------------------------------------------
# Subdirectories
# -------------------------------------------------------------

add_subdirectory("time_series")
add_subdirectory("util/cart_grid")

include_directories(BEFORE ${CMAKE_CURRENT_BINARY_DIR}/time_series )

# -------------------------------------------------------------
# mass2
# -------------------------------------------------------------
set(mass2_src
  "${CMAKE_CURRENT_BINARY_DIR}/banner.f90"
  bed.f90 
  bed_functions.f90 
  bed_var.f90 
  block_grid.f90 
  block_hydro.f90 
  block_hydro_bc.f90 
  block_initialization.f90 
  block_parallel.f90 
  block_var.f90 
  block_var_base.f90 
  differencing.f90 
  distance.f90 
  energy_flux_module.f90 
  gage_output_module.f90 
  gas_coeffs_module.f90 
  gas_functions_module.f90 
  generic_source.f90 
  globals.f90 
  hotstart.f90 
  hydro_bc.f90 
  hydro_solve.f90 
  mass2_config.f90 
  mass2_parallel.f90 
  mass_source_output.f90 
  met_data_module.f90 
  particulate_source.f90 
  plot_cgns.f90 
  plot_output.f90 
  profile_init.f90 
  realequal.f90 
  scalar_bc.f90 
  scalar_mass_balance.f90 
  scalar_solve.f90 
  scalars_module.f90 
  scalars_source.f90 
  sediment_source.f90 
  solver_common.f90 
  solver_petsc.F90
  table_bc_module.f90 
  tdg_source.f90 
  temperature_source.f90 
  transport_only_module.f90
  )
set_source_files_properties(
  ${mass2_src}
  PROPERTIES Fortran_PREPROCESS ON
)

add_executable(mass2 ${mass2_src} )
target_link_libraries(mass2
  ts
  ${CGNS_LIBRARIES}
  ${PETSc_LIBRARIES}
  ${GA_LIBRARIES}
  ${MPI_Fortran_LIBRARIES}
  )

# -------------------------------------------------------------
# solver_test
# -------------------------------------------------------------
add_executable(solver_test
  solver_test.f90 
  solver_common.f90 
  solver_backup.f90 
  solver_petsc.F90
  )
target_link_libraries(solver_test
  ts
  ${PETSc_LIBRARIES}
  ${MPI_Fortran_LIBRARIES}
  )
