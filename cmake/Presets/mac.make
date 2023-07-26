# --- Presets that explicitly request CUSTOM compilers --------------- #

set( CMAKE_Fortran_COMPILER "/opt/openmpi-3.1.6/bin/mpif90" CACHE STRING "" FORCE )
set( CMAKE_C_COMPILER       "/opt/openmpi-3.1.6/bin/mpicc"  CACHE STRING "" FORCE )
set( CMAKE_CXX_COMPILER     "/opt/openmpi-3.1.6/bin/mpicxx" CACHE STRING "" FORCE )

set( MPI_Fortran_COMPILER   "/opt/openmpi-3.1.6/bin/mpif90" CACHE STRING "" FORCE )
set( MPI_C_COMPILER         "/opt/openmpi-3.1.6/bin/mpicc"  CACHE STRING "" FORCE )
set( MPI_CXX_COMPILER       "/opt/openmpi-3.1.6/bin/mpicxx" CACHE STRING "" FORCE )

# --- End of the file ------------------------------------------------ #
