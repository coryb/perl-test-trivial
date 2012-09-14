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
	perl Makefile.PL

test dist: Makefile
	${MAKE} -f Makefile $@

fail:
	curl http://www.cpantesters.org/distro/T/Test-Trivial.json | jshon -a -e ostext -u -p -e osvers -u -p -e perl -u -p -e status -u| while read o; read v; read p; read s; do echo "$$o-$$v [$$p]: $$s"; done | grep FAIL

pass:
	curl http://www.cpantesters.org/distro/T/Test-Trivial.json | jshon -a -e ostext -u -p -e osvers -u -p -e perl -u -p -e platform -u -p -e status -u| while read o; read v; read p; read plat; read s; do echo "$$o-$$v [$$p $$plat]: $$s"; done | grep PASS

.PHONY: readme clean upload test dist