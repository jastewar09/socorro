# --- Presets that explicitly request GCC compilers ------------------ #

set( CMAKE_Fortran_COMPILER "gfortran" CACHE STRING "" FORCE )
set( CMAKE_C_COMPILER       "gcc"      CACHE STRING "" FORCE )
set( CMAKE_CXX_COMPILER     "g++"      CACHE STRING "" FORCE )

set( MPI_Fortran_COMPILER   "mpif90"   CACHE STRING "" FORCE )
set( MPI_C_COMPILER         "mpicc"    CACHE STRING "" FORCE )
set( MPI_CXX_COMPILER       "mpicxx"   CACHE STRING "" FORCE )

# --- End of the file ------------------------------------------------ #
