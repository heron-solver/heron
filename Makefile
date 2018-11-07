CC=mlton

all:
	cd src && mllex parse.lex
	cd src && mlyacc parse.grm
	${CC} -verbose 1 -output heron src/heron.mlb

binary-release: all
	tar czvf heron-$(shell /bin/echo | ./heron | grep Heron | cut -d ' ' -f 2)-$(shell uname -m)-$(shell uname | tr A-Z a-z).tar.gz \
		heron \
		lib \
		examples/HandWatch* \
		examples/Radiotherapy.tesl \
		examples/PowerWindow.tesl \
		examples/basic \
		examples/reference-gallery \
		examples/takeoff-procedures

# Tests are done with TESL files and expected outputs available in ./tests
test:
	rm -f tests/*.out tests/_results.log
	chmod +x tests/check.sh
	for var in $(shell ls tests/*.tesl) ; do tests/check.sh $${var} ; done | tee tests/_results.log
	if (! (grep "FAIL" tests/_results.log >/dev/null)) ; then echo "\e[1m\e[32mCongrats! ALL TESTS PASSED.\e[0m" ; else echo "\e[1m\e[31mSorry! SOME TEST HAS FAILED.\e[0m" ; exit 1 ; fi

clean:
	rm -f tests/*.out tests/_results.log
	rm -f src/parse.grm.desc src/parse.grm.sig src/parse.grm.sml src/parse.lex.sml

# with_polyml:
#  	cd src && mllex parse.lex
#  	cd src && mlyacc parse.grm
#  	./contrib/sml-buildscripts/polybuild src/heron.mlb
#  	mv src/heron .
