CC=ocamlopt

all:
	${CC} src/TESL.ml

binary-release: all
	tar czvf heron-$(shell /bin/echo | ./heron | grep Heron | cut -d ' ' -f 2)-$(shell uname -m)-$(shell uname | tr A-Z a-z).tar.gz \
		heron \
		examples/HandWatch* \
		examples/Radiotherapy.tesl \
		examples/AirplaneTakeoff.tesl \
		examples/PowerWindow.tesl \
		examples/basic \
		examples/reference-gallery

clean:
	ls
