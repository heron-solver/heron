run:
	mlton tesl-symx.mlb
	./tesl-symx

run_polyml:
	./contrib/sml-buildscripts/polybuild tesl-symx.mlb
	./tesl-symx
