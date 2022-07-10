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
# Last Change: 2022-07-10 07:47:56 perk
# -------------------------------------------------------------

cmake_minimum_required(VERSION 3.16)

project(MASS2)

enable_language(Fortran)

# -------------------------------------------------------------
# MPI compiler
# -------------------------------------------------------------
message(STATUS "Checking MPI ...")
find_package (MPI REQUIRED)
message(STATUS "MPI_Fortran_LIBRARIES: ${MPI_Fortran_LIBRARIES}")

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

include_directories (AFTER ${PETSc_INCLUDES})

# -------------------------------------------------------------
# Subdirectories
# -------------------------------------------------------------

add_subdirectory("time_series")
add_subdirectory("util/cart_grid")

include_directories(BEFORE
  ${CMAKE_CURRENT_BINARY_DIR}/time_series
  )

