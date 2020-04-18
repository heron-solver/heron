CC=mlton
CC_multi=mpl
ARCH=$(shell uname -m)
OS=$(shell uname | tr A-Z a-z)
HERON_VERSION=$(shell cat src/VERSION)

all: clean
	if (which mpl) ; then make multicore ; else if (which mlton) ; then make singlecore ; else echo "Neither MPL or MLton has been found." ; fi ; fi

# To compile with MLton
singlecore: parser
	sed 's/_COMPILER_CMD_/${CC}/g' src/Toplevel.sml > src/_Toplevel.sml
	sed 's/_VERSION_/${HERON_VERSION}/g' src/_Toplevel.sml > src/__Toplevel.sml
	cp src/heron-Singlecore.mlb src/heron.mlb
	${CC} -verbose 1 -output heron src/heron.mlb

# To compile with MPL
multicore: parser
	sed 's/_COMPILER_CMD_/${CC_multi}/g' src/Toplevel.sml > src/_Toplevel.sml
	sed 's/_VERSION_/${HERON_VERSION}/g' src/_Toplevel.sml > src/__Toplevel.sml
	cp src/heron-Multicore.mlb src/heron.mlb
	${CC_multi} -verbose 1 -output heron src/heron.mlb

parser:
	cd src && mllex parse.lex
	cd src && mlyacc parse.grm

binary-release: all
	tar czvf heron-$(shell /bin/echo | ./heron | grep Heron | cut -d ' ' -f 2)-${ARCH}-${OS}.tar.gz \
		heron \
		lib \
		examples/

# Tests are done with TESL files and expected outputs available in ./regression
test:
	@echo Starting test session...
	@rm -f regression/*.out regression/_results.log
	@chmod +x regression/sysinfo.sh regression/check.sh regression/decide.sh
	@regression/sysinfo.sh
	@for var in $(shell ls regression/*.tesl) ; do regression/check.sh $${var} ; done | tee regression/_results.log
	@rm -f regression/*.sorted
	@regression/decide.sh

clean:
	rm -f regression/*.out regression/_results.log
	rm -f ./heron src/parse.grm.desc src/parse.grm.sig src/parse.grm.sml src/parse.lex.sml src/_Toplevel.sml src/__Toplevel.sml src/heron.mlb

# with_polyml:
#  	cd src && mllex parse.lex
#  	cd src && mlyacc parse.grm
#  	./contrib/sml-buildscripts/polybuild src/heron.mlb
#  	mv src/heron .
