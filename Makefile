run:
	mlton -output heron src/heron.mlb
	./heron

run_polyml:
	./contrib/sml-buildscripts/polybuild src/heron.mlb
	mv src/heron .
	./heron
