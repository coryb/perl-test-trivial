readme:
	pod2text lib/Test/Trivial.pm > README

%:
	${MAKE} -f Makefile $@