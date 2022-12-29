# ----- MINPACK Makefile --------------------------------------------- #

SHELL	= /bin/bash

# ----- Definitions -------------------------------------------------- #

LIB	= libminpack.a

SRC	= $(shell ls *.f)
OBJ	= $(SRC:.f=.o)

# ----- Compiler and linker settings --------------------------------- #

FC	= gfortran
FCFLAGS	= -O2 -frecursive

AR	= ar
ARFLAGS	= -cr

# ----- Remove build files ------------------------------------------- #

clean	:
	@ rm -rf *.o $(LIB)

# ----- Library rules ------------------------------------------------ #

minpack : $(OBJ)
	$(AR) $(ARFLAGS) $(LIB) $^

# ----- Compilation rules -------------------------------------------- #

%.o	: %.f
	$(FC) $(FCFLAGS) -c $<

# ----- End of the makefile ------------------------------------------ #
