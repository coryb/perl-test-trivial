readme:
	pod2text lib/Test/Trivial.pm > README

clean:
	[ ! -e ./Makefile ] || ${MAKE} -f Makefile clean
	rm -rf Makefile Makefile.old blib pm_to_blib *.tar.gz

TGZ=Test-Trivial-$(shell perl -MExtUtils::MakeMaker -le 'print MM->parse_version(shift)' 'lib/Test/Trivial.pm').tar.gz

$(TGZ): dist

upload: $(TGZ)
	cpan-upload -v $(TGZ) --user $(USER)

Makefile:
	/usr/bin/perl Makefile.PL

test dist: Makefile
	${MAKE} -f Makefile $@

.PHONY: readme clean upload test dist