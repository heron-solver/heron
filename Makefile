run_poly:
	./contrib/polybuild tesl-symx.mlb
	./tesl-symx

run_mlton:
	mlton tesl-symx.mlb
	./tesl-symx
