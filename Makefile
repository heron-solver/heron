MLTON=mlton
MPL=mpl
ARCH=$(shell uname -m)
OS=$(shell uname | tr A-Z a-z)
HERON_VERSION=$(shell cat VERSION)

all: heron hmc

# Main solver
heron: clean
	if (which mpl) ; then make with-mpl ; else if (which mlton) ; then make with-mlton ; else echo "Neither MPL or MLton has been found." ; fi ; fi

# To compile with MLton
with-mlton: TESL-parser
	sed 's/_COMPILER_CMD_/${MLTON}/g' src/heron/Toplevel.sml > src/heron/_Toplevel.sml
	sed 's/_VERSION_/${HERON_VERSION}/g' src/heron/_Toplevel.sml > src/heron/__Toplevel.sml
	cp src/heron/heron-Singlecore.mlb src/heron/heron.mlb
	${MLTON} -verbose 1 -output heron src/heron/heron.mlb

# To compile with MPL
with-mpl: TESL-parser
	sed 's/_COMPILER_CMD_/${MPL}/g' src/heron/Toplevel.sml > src/heron/_Toplevel.sml
	sed 's/_VERSION_/${HERON_VERSION}/g' src/heron/_Toplevel.sml > src/heron/__Toplevel.sml
	cp src/heron/heron-Multicore.mlb src/heron/heron.mlb
	${MPL} -verbose 1 -output heron src/heron/heron.mlb

TESL-parser:
	cd src/heron && mllex TESL-parse.lex
	cd src/heron && mlyacc TESL-parse.grm

# Model-checker
hmc: TESL-parser src/hmc/*
	sed 's/_VERSION_/${HERON_VERSION}/g' src/hmc/ModelChecker.sml > src/hmc/_ModelChecker.sml
	${MLTON} -verbose 1 -output hmc src/hmc/hmc.mlb

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
	rm -f ./heron src/heron/TESL-parse.grm.desc src/heron/TESL-parse.grm.sig src/heron/TESL-parse.grm.sml src/heron/TESL-parse.lex.sml src/heron/_Toplevel.sml src/heron/__Toplevel.sml src/hmc/_ModelChecker.sml src/heron/heron.mlb

# with_polyml:
#  	cd src && mllex parse.lex
#  	cd src && mlyacc parse.grm
#  	./contrib/sml-buildscripts/polybuild src/heron.mlb
#  	mv src/heron .
