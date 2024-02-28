# ----- Multi-machine makefile --------------------------------------- #

SHELL	= /bin/bash
PYTHON	= python

# ----- Definitions -------------------------------------------------- #

EXE	= socorro

FSRC	= $(shell ls *.F90)
CSRC	= $(shell ls *.c)
OBJS	= $(FSRC:.F90=.o) $(CSRC:.c=.o)

BIN_DIR	= ../build
OBJ_DIR	= Obj_build
LIB_DIR	= $(@:lib-%=%)

# ----- Help message ------------------------------------------------- #

help	:
	@ echo ''
	@ echo 'Execute any of the following make commands from the src directory:'
	@ echo ''
	@ echo 'make doc                     Create the documentation files for socorro'
	@ echo 'make git                     Print available git version information'
	@ echo 'make tar                     Create socorro_src.tar.gz for the src dir'
	@ echo ''
	@ echo 'make clean                   Delete the build related files in src'
	@ echo 'make purge                   Delete all build related files'
	@ echo ''
	@ echo 'make lib-package             Print help message to download/build a package in ../lib'
	@ echo 'make lib-package args="..."  Download/build a package in ../lib'
	@ echo ''
	@ echo 'make depends                 Build the Fortran module dependency list'
	@ echo 'make settings                Build socorro using Makefile.settings'
	@ echo 'make install                 Install socorro in ../build'
	@ echo ''
	@ echo 'where Makefile.settings is one of these files from src/MAKE/OPTIONS:'
	@ echo ''
	@ ( cd MAKE/OPTIONS/ ; ls -1 Makefile.* 2>/dev/null ; cd ../ )
	@ echo ''
	@ echo '... or one of these files from src/MAKE/MACHINES'
	@ echo ''
	@ ( cd MAKE/MACHINES/ ; ls -1 Makefile.* 2>/dev/null ; cd ../ )
	@ echo ''
	@ echo '... or one of these files from src/MAKE/MINE'
	@ echo ''
	@ ( cd MAKE/MINE/ ; ls -1 Makefile.* 2>/dev/null ; cd ../ )
	@ echo ''
	@ echo '... or a new Makefile.settings can be created and placed in src/MAKE/MINE for use'
	@ echo ''

# ----- Create documentation ----------------------------------------- #

doc	:
	@ echo ''
	@ echo 'ERROR: Documentation not yet available.'

# ----- Print git-based version information -------------------------- #

git	:
	@ echo ''
	@ echo 'Gathering git-based version information ...'
	@ if (type git && test -e ../.git) >> /dev/null 2>> /dev/null ; then \
	    echo 'branch  =' $$(git rev-parse --abbrev-ref HEAD) ; \
	    echo 'commit  =' $$(git rev-parse HEAD) ; \
	  else \
	    echo 'branch  =' unknown ; \
	    echo 'commit  =' unknown ; \
	  fi ;

# ----- Create an archive of the src dir ----------------------------- #

tar	:
	@ echo ''
	@ echo 'Creating socorro_src.tar.gz ...'
	@ make clean &>/dev/null && cd ../ && tar -czf socorro_src.tar.gz src
	@ echo 'Finished socorro_src.tar.gz'

# ----- Cleanup build files ------------------------------------------ #

clean	:
	@ echo ''
	@ echo 'Removing the build related files in src ...'
	@ rm -rf $(OBJ_DIR) $(EXE) Makefile.depends

purge	:
	@ echo ''
	@ echo 'Removing all build related files ...'
	@ rm -rf $(OBJ_DIR) $(EXE) Makefile.depends $(BIN_DIR)/*

# ----- Build libraries ---------------------------------------------- #

lib-%	:
	@ if [ -f ../lib/$(LIB_DIR)/Install.py ] ; then \
	  echo '' ; \
	  echo 'Executing ../lib/$(@:lib-%=%)/Install.py' ; \
	  ( cd ../lib/$(LIB_DIR) ; $(PYTHON) Install.py $(args) ) ; \
	else \
	  echo '' ; \
	  echo 'ERROR: Install.py file for lib $(@:lib-%=%) does not exist' ; \
	fi ; touch main.F90

# ----- Compile and install ------------------------------------------ #

depends	:
	@ echo ''
	@ echo 'Generating the Fortran module dependency list ...'
	@ rm -rf Makefile.depends ; MAKE/TOOLS/f90makedep.pl $(FSRC)

install	:
	@ if [ ! -f $(EXE) ] ; then \
	  echo '' ; \
	  echo 'ERROR: Executable $(EXE) does not exist' ; exit ; \
	else \
	  echo '' ; \
	  echo 'Installing $(EXE) in $(BIN_DIR)' ; \
	  if [ ! -d $(BIN_DIR) ] ; then mkdir $(BIN_DIR) ; fi ; \
	  mv $(EXE) $(BIN_DIR) ; \
	fi

.DEFAULT:
	@ test -f MAKE/MACHINES/Makefile.$@ -o \
	       -f MAKE/MINE/Makefile.$@ -o \
	       -f MAKE/OPTIONS/Makefile.$@
	@ if [ ! -d $(OBJ_DIR) ] ; then mkdir $(OBJ_DIR) ; fi
	@ echo 'Gathering build information ...'
	@ if [ -f MAKE/MACHINES/Makefile.$@ ] ; \
	  then cp MAKE/MACHINES/Makefile.$@ $(OBJ_DIR)/Makefile ; fi
	@ if [ -f MAKE/MINE/Makefile.$@ ] ; \
	  then cp MAKE/MINE/Makefile.$@ $(OBJ_DIR)/Makefile ; fi
	@ if [ -f MAKE/OPTIONS/Makefile.$@ ] ; \
	  then cp MAKE/OPTIONS/Makefile.$@ $(OBJ_DIR)/Makefile ; fi
	@ make depends &>/dev/null ; mv Makefile.depends $(OBJ_DIR)
	@ echo "Compiling Socorro with Makefile.$@ settings ..."
	@ cd $(OBJ_DIR) ; $(MAKE) "OBJ = $(OBJS)" "EXE = ../$(EXE)"
	@ make install

# ----- End of the makefile ------------------------------------------ #
