This directory contains the external libraries that are required for
building and using Socorro.

The libraries themselves must first be intalled and/or built, so that
the appropriate library files exist for Socorro to link against. Each
subdirectory contains a python helper script to assist in downloading
and building the library with reasonable compiler options. By running
"python Install.py" or "python Install.py -h" from these directories,
you can get detailed help information, or you can type "make lib-pkg"
from the src directory to do the same thing, or you can build each of
the libraries manually by following their instructions. Note that pkg
in "make lib-pkg" is one of the library names below.

The libraries contained in this directory are the following:

- fftw2       URL: http://www.fftw.org/
- fftw3       URL: http://www.fftw.org/
- lapack      URL: http://www.netlib.org/lapack/
- libxc       URL: https://tddft.org/programs/libxc/
- minpack     URL: https://netlib.org/minpack/
- scalapack   URL: http://www.netlib.org/scalapack/
