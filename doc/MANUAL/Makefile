#include ../make.conf

derp	:
	perl makedocs2 ../src/*_mod.F90; \
	mv ../src/*.doc .

all	:
#	perl makedocs ../src/*_mod.F; \
	perl makedocs2 ../src/*_mod.F90; \
	mv ../src/*.doc .
#	( cd LATEX ; make all )

clean	:
	rm -f *.doc
	( cd LATEX ; make clean )
