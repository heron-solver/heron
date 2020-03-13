CC=mlton
CC_multi=mpl
ARCH=$(shell uname -m)
OS=$(shell uname | tr A-Z a-z)

all: clean
	if [ "${OS}" = "linux" ]  ; then make multicore ; fi
	if [ "${OS}" = "darwin" ] ; then make singlecore ; fi

singlecore: parser
	sed 's/_COMPILER_CMD_/${CC}/g' src/Toplevel.sml > src/_Toplevel.sml
	cp src/heron-Singlecore.mlb src/heron.mlb
	${CC} -verbose 1 -output heron src/heron.mlb

multicore: parser
	sed 's/_COMPILER_CMD_/${CC_multi}/g' src/Toplevel.sml > src/_Toplevel.sml
	cp src/heron-Multicore.mlb src/heron.mlb
	${CC_multi} -verbose 1 -output heron src/heron.mlb

parser:
	cd src && mllex parse.lex
	cd src && mlyacc parse.grm

binary-release: all
	tar czvf heron-$(shell /bin/echo | ./heron | grep Heron | cut -d ' ' -f 2)-${ARCH}-${OS}.tar.gz \
		heron \
		lib \
		examples/HandWatch* \
		examples/Radiotherapy.tesl \
		examples/PowerWindow.tesl \
		examples/basic \
		examples/reference-gallery \
		examples/aviation

# Tests are done with TESL files and expected outputs available in ./regression
test:
	rm -f regression/*.out regression/_results.log
	chmod +x regression/check.sh
	for var in $(shell ls regression/*.tesl) ; do regression/check.sh $${var} ; done | tee regression/_results.log
	rm -f regression/*.sorted
	if (! (grep "FAIL" regression/_results.log >/dev/null)) ; then echo "\e[1m\e[32mCongrats! ALL REGRESSION TESTS PASSED.\e[0m" ; else echo "\e[1m\e[31mSorry! SOME REGRESSION TESTS HAVE FAILED.\e[0m" ; exit 1 ; fi

clean:
	rm -f regression/*.out regression/_results.log
	rm -f ./heron src/parse.grm.desc src/parse.grm.sig src/parse.grm.sml src/parse.lex.sml src/_Toplevel.sml src/heron.mlb

# with_polyml:
#  	cd src && mllex parse.lex
#  	cd src && mlyacc parse.grm
#  	./contrib/sml-buildscripts/polybuild src/heron.mlb
#  	mv src/heron .
