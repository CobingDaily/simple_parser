all:
	dune build
	./_build/default/main.exe

clean:
	rm -rf _build/
